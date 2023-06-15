module Bin.Run where

import Prelude

import Bin.AppM (AppM)
import Bin.AppM as AppM
import Control.Alternative (guard)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Lib.Foreign.Octokit (Release, ReleaseAsset)
import Lib.Foreign.Octokit as Octokit
import Lib.GitHub as GitHub
import Lib.Nix.Manifest (FetchUrl, NamedManifest, PursManifest)
import Lib.Nix.Prefetch as Nix.Prefetch
import Lib.Nix.System (NixSystem(..))
import Lib.Nix.System as NixSystem
import Lib.SemVer (SemVer(..))
import Lib.SemVer as SemVer
import Lib.Tool (Channel(..), Tool(..), ToolChannel(..), ToolPackage(..))
import Lib.Tool as Tool
import Node.Process as Process
import Partial.Unsafe (unsafeCrashWith)
import Registry.Version as Version

updatePurs :: AppM Unit
updatePurs = do
  manifest <- AppM.readPursManifest

  let
    existing :: Set SemVer
    existing = Set.unions $ map Map.keys $ Map.values manifest

  rawReleases <- AppM.runGitHubM (GitHub.listReleases Purs) >>= case _ of
    Left error -> do
      Console.log $ "Failed to fetch releases for " <> Tool.print Purs Tool.Executable
      Console.log $ Octokit.printGitHubError error
      liftEffect $ Process.exit 1
    Right releases -> pure releases

  let
    parsePursReleases :: Array Release -> Map SemVer (Map NixSystem String)
    parsePursReleases = Map.fromFoldable <<< Array.mapMaybe parsePursRelease

    parsePursRelease :: Release -> Maybe (Tuple SemVer (Map NixSystem String))
    parsePursRelease release = Either.hush do
      version <- SemVer.parse (fromMaybe release.tag (String.stripPrefix (String.Pattern "v") release.tag))

      let
        parseAsset :: ReleaseAsset -> Maybe (Tuple NixSystem String)
        parseAsset asset = do
          guard $ isJust $ String.stripSuffix (String.Pattern ".tar.gz") asset.name
          guard $ not $ String.contains (String.Pattern "win64") asset.name
          system <- Either.hush $ NixSystem.fromPursReleaseTarball asset.name
          pure $ Tuple system asset.downloadUrl

        supportedAssets :: Map NixSystem String
        supportedAssets = Map.fromFoldable $ Array.mapMaybe parseAsset release.assets

      pure $ Tuple version supportedAssets

    -- Darwin prereleases from 0.15.0 to 0.15.9 have incorrect version numbers
    isBrokenDarwinPreRelease :: SemVer -> NixSystem -> Boolean
    isBrokenDarwinPreRelease (SemVer { version, pre }) system =
      if system == X86_64_darwin && Version.major version == 0 && Version.minor version == 15 then case pre of
        Nothing -> false
        Just _ -> Version.patch version < 10
      else false

    -- We only accept compiler releases back to 0.13
    isBeforeCutoff :: SemVer -> Boolean
    isBeforeCutoff (SemVer { version }) = Version.major version == 0 && Version.minor version < 13

    -- Some versions are broken and we don't want to include them
    isBrokenVersion :: SemVer -> Boolean
    isBrokenVersion (SemVer { version }) = Array.elem version
      [ unsafeVersion "0.13.1" -- https://github.com/purescript/purescript/releases/tag/v0.13.1 (doesn't work)
      , unsafeVersion "0.13.7" -- https://github.com/purescript/purescript/releases/tag/v0.13.7 (has no releases)
      , unsafeVersion "0.15.1" -- https://github.com/purescript/purescript/releases/tag/v0.15.1 (incorrect version number, identical to 0.15.2)
      ]
      where
      unsafeVersion = Either.fromRight' (\_ -> unsafeCrashWith "Unexpected Left in isBroken") <<< Version.parse

    -- All releases that ought to be included in the resulting manifest
    supportedReleases :: Map SemVer (Map NixSystem String)
    supportedReleases = parsePursReleases rawReleases # Map.mapMaybeWithKey \version assets -> do
      guard $ not $ isBrokenVersion version || isBeforeCutoff version
      pure $ Map.filterKeys (isBrokenDarwinPreRelease version) assets

    -- We only want to include releases that aren't already present in the
    -- manifest file.
    newReleases :: Map SemVer (Map NixSystem String)
    newReleases = Map.filterKeys (not <<< flip Set.member existing) supportedReleases

  hashedReleases :: Map SemVer (Map NixSystem FetchUrl) <-
    for newReleases \assets ->
      for assets \asset ->
        Nix.Prefetch.nixPrefetchTarball asset >>= case _ of
          Left error -> do
            Console.log $ "Failed to hash release asset at url " <> asset <> ": " <> error
            liftEffect $ Process.exit 1
          Right hash -> pure { url: asset, hash }

  let
    -- Create a new release manifest containing the new updates
    newReleaseManifest :: PursManifest
    newReleaseManifest = do
      let
        flatReleases :: Array { version :: SemVer, system :: NixSystem, fetch :: FetchUrl }
        flatReleases =
          Array.concatMap
            (\(Tuple version inner) -> map (\(Tuple system fetch) -> { version, system, fetch }) (Map.toUnfoldable inner))
            (Map.toUnfoldable hashedReleases)

      Array.foldl (\acc val -> Map.insertWith Map.union val.system (Map.singleton val.version val.fetch) acc) manifest flatReleases

  -- Write the new release manifest to disk
  when (newReleaseManifest /= manifest) do
    AppM.writePursManifest newReleaseManifest

  -- Update the named manifests, if necessary.
  named <- AppM.readNamedManifest

  let
    named' :: NamedManifest
    named' = fromMaybe named do
      let unstableChannel = ToolChannel { tool: Purs, channel: Unstable }
      let stableChannel = ToolChannel { tool: Purs, channel: Stable }

      ToolPackage { version: unstable } <- Map.lookup unstableChannel named
      ToolPackage { version: stable } <- Map.lookup stableChannel named

      let allVersions = Set.unions $ map Map.keys $ Map.values newReleaseManifest
      let allStableVersions = Set.filter (\(SemVer { pre }) -> isNothing pre) allVersions

      maxUnstable <- Set.findMax allVersions
      maxStable <- Set.findMax allStableVersions

      let insertPackage channel version = Map.insert channel (ToolPackage { tool: Purs, version })
      let updateUnstable prev = if maxUnstable > unstable then insertPackage unstableChannel maxUnstable prev else prev
      let updateStable prev = if maxStable > stable then insertPackage stableChannel maxStable prev else prev

      pure (updateStable (updateUnstable named))

  when (named /= named') do
    AppM.writeNamedManifest named'

  pure unit
