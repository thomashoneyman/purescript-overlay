module Bin.Run where

import Prelude

import Bin.AppM (AppM)
import Bin.AppM as AppM
import Control.Alternative (guard)
import Data.Array as Array
import Data.Either (Either(..), fromRight')
import Data.Either as Either
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (alaF, un)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Traversable (foldMap, for)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Lib.Foreign.Octokit (Release, ReleaseAsset)
import Lib.Foreign.Octokit as Octokit
import Lib.Git as Git
import Lib.GitHub as GitHub
import Lib.Nix.Manifest (FetchUrl, NamedManifest, PursManifest, SpagoManifest, PursTidyManifest)
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

verifyPurs :: AppM Unit
verifyPurs = do
  manifest <- AppM.readPursManifest
  let entries = alaF Additive foldMap Map.size (Map.values manifest)
  Console.log $ "Successfully parsed purs.json with " <> Int.toStringAs Int.decimal entries <> " entries."

verifySpago :: AppM Unit
verifySpago = do
  manifest <- AppM.readSpagoManifest
  let entries = Map.size manifest
  Console.log $ "Successfully parsed purs.json with " <> Int.toStringAs Int.decimal entries <> " entries."

prefetchPurs :: AppM PursManifest
prefetchPurs = do
  manifest <- AppM.readPursManifest

  let
    existing :: Set SemVer
    existing = Map.keys manifest

  rawReleases <- AppM.runGitHubM (GitHub.listReleases Purs) >>= case _ of
    Left error -> do
      Console.log $ "Failed to fetch releases for " <> Tool.print Purs Tool.Executable
      Console.log $ Octokit.printGitHubError error
      liftEffect $ Process.exit 1
    Right releases -> pure releases

  Console.log $ "Retrieved " <> show (Array.length rawReleases) <> " releases for " <> Tool.print Purs Tool.Executable

  let
    parsePursReleases :: Array Release -> Map SemVer (Map NixSystem String)
    parsePursReleases = Map.fromFoldable <<< Array.mapMaybe \release -> Either.hush do
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
      guard $ not $ isBrokenVersion version
      guard $ not $ isBeforeCutoff version
      pure $ Map.filterKeys (not <<< isBrokenDarwinPreRelease version) assets

    -- We only want to include releases that aren't already present in the
    -- manifest file.
    newReleases :: Map SemVer (Map NixSystem String)
    newReleases = Map.filterKeys (not <<< flip Set.member existing) supportedReleases

  Console.log $ "Found " <> show (Map.size newReleases) <> " new releases for " <> Tool.print Purs Tool.Executable

  hashedReleases :: PursManifest <- forWithIndex newReleases \version assets -> do
    Console.log $ "Processing release " <> SemVer.print version
    Console.log $ "Fetching hashes for release assets"
    for assets \asset -> do
      Console.log $ "  " <> asset
      Nix.Prefetch.nixPrefetchTarball asset >>= case _ of
        Left error -> do
          Console.log $ "Failed to hash release asset at url " <> asset <> ": " <> error
          liftEffect $ Process.exit 1
        Right hash -> pure { url: asset, hash }

  pure hashedReleases

writePursUpdates :: PursManifest -> AppM Unit
writePursUpdates updates = do
  manifest <- AppM.readPursManifest
  let newManifest = Map.unionWith Map.union manifest updates
  AppM.writePursManifest newManifest
  named <- AppM.readNamedManifest

  let
    named' :: Maybe NamedManifest
    named' = do
      let unstableChannel = ToolChannel { tool: Purs, channel: Unstable }
      let stableChannel = ToolChannel { tool: Purs, channel: Stable }

      ToolPackage { version: unstable } <- Map.lookup unstableChannel named
      ToolPackage { version: stable } <- Map.lookup stableChannel named

      let
        allVersions = Map.keys newManifest
        allStableVersions = Set.filter (\(SemVer { pre }) -> isNothing pre) allVersions
        maxUnstable = Set.findMax allVersions
        maxStable = Set.findMax allStableVersions

        insertPackage :: ToolChannel -> SemVer -> NamedManifest -> NamedManifest
        insertPackage channel version = Map.insert channel (ToolPackage { tool: Purs, version })

        updateUnstable :: NamedManifest -> NamedManifest
        updateUnstable prev = case maxUnstable of
          Just version | version > unstable -> insertPackage unstableChannel version prev
          _ -> prev

        updateStable :: NamedManifest -> NamedManifest
        updateStable prev = case maxStable of
          Just version | version > stable -> insertPackage stableChannel version prev
          _ -> prev

      pure (updateStable (updateUnstable named))

  case named' of
    Nothing -> Console.log "Failed to update named manifest" *> liftEffect (Process.exit 1)
    Just result -> AppM.writeNamedManifest result

prefetchSpago :: AppM SpagoManifest
prefetchSpago = do
  manifest <- AppM.readSpagoManifest

  let
    existing :: Set SemVer
    existing = Map.keys manifest

  rawReleases <- AppM.runGitHubM (GitHub.listReleases Spago) >>= case _ of
    Left error -> do
      Console.log $ "Failed to fetch releases for " <> Tool.print Spago Tool.Executable
      Console.log $ Octokit.printGitHubError error
      liftEffect $ Process.exit 1
    Right releases -> pure releases

  Console.log $ "Retrieved " <> show (Array.length rawReleases) <> " releases for " <> Tool.print Spago Tool.Executable

  let
    parseSpagoReleases :: Array Release -> Map SemVer (Either Git.Tag (Map NixSystem String))
    parseSpagoReleases = Map.fromFoldable <<< Array.mapMaybe \release -> Either.hush do
      version <- SemVer.parse (fromMaybe release.tag (String.stripPrefix (String.Pattern "v") release.tag))

      -- Spaghetto versions begin at 0.90.0
      let minGitRef = fromRight' (\_ -> unsafeCrashWith "bad version") (Version.parse "0.90.0")
      let minSpaghetto = SemVer { version: minGitRef, pre: Nothing }
      if version >= minSpaghetto then
        pure $ Tuple version $ Left $ Git.Tag release.tag
      else do
        let
          parseAsset :: ReleaseAsset -> Maybe (Tuple NixSystem String)
          parseAsset asset = do
            trimmed <- String.stripSuffix (String.Pattern ".tar.gz") asset.name
            guard $ not $ Array.elem trimmed [ "Windows", "windows" ]
            system <- Either.hush $ NixSystem.fromSpagoReleaseTarball asset.name
            pure $ Tuple system asset.downloadUrl

          supportedAssets :: Map NixSystem String
          supportedAssets = Map.fromFoldable $ Array.mapMaybe parseAsset release.assets

        pure $ Tuple version $ Right supportedAssets

    -- We only accept Spago releases back to 0.18, which is the first one with
    -- a configurable cache (so we don't get Nix errors trying to run Spago)
    isBeforeCutoff :: SemVer -> Boolean
    isBeforeCutoff (SemVer { version }) =
      Version.major version == 0 && Version.minor version < 18

    -- Some versions are broken and we don't want to include them
    isBrokenVersion :: SemVer -> Boolean
    isBrokenVersion (SemVer { version }) = Array.elem version
      [ unsafeVersion "0.19.2" -- wrong version number
      ]
      where
      unsafeVersion = Either.fromRight' (\_ -> unsafeCrashWith "Unexpected Left in isBroken") <<< Version.parse

    supportedReleases :: Map SemVer (Either Git.Tag (Map NixSystem String))
    supportedReleases = parseSpagoReleases rawReleases # Map.mapMaybeWithKey \version entry -> do
      guard $ not $ isBeforeCutoff version
      guard $ not $ isBrokenVersion version
      guard $ not $ isJust (un SemVer version).pre
      pure entry

    -- We only want to include releases that aren't already present in the
    -- manifest file.
    newReleases :: Map SemVer (Either Git.Tag (Map NixSystem String))
    newReleases = Map.filterKeys (not <<< flip Set.member existing) supportedReleases

  Console.log $ "Found " <> show (Map.size newReleases) <> " new releases for " <> Tool.print Spago Tool.Executable

  hashedReleases :: SpagoManifest <- forWithIndex newReleases \version entry -> do
    Console.log $ "Processing release " <> SemVer.print version
    case entry of
      Left (Git.Tag tag) -> do
        Console.log $ "Fetching NPM tarball associated with tag " <> tag <> ", ie. version " <> SemVer.print version
        let url = "https://registry.npmjs.org/spago/-/spago-" <> SemVer.print version <> ".tgz"
        Nix.Prefetch.nixPrefetchTarball url >>= case _ of
          Left error -> do
            Console.log $ "Failed to hash NPM tarbal at url " <> url <> ": " <> error
            liftEffect $ Process.exit 1
          Right hash -> pure $ Left { url, hash }
      Right assets -> do
        Console.log $ "Fetching hashes for release assets"
        hashed <- for assets \asset -> do
          Console.log $ "  " <> asset
          Nix.Prefetch.nixPrefetchTarball asset >>= case _ of
            Left error -> do
              Console.log $ "Failed to hash release asset at url " <> asset <> ": " <> error
              liftEffect $ Process.exit 1
            Right hash -> pure { url: asset, hash }
        pure $ Right hashed

  pure hashedReleases

writeSpagoUpdates :: SpagoManifest -> AppM Unit
writeSpagoUpdates updates = do
  manifest <- AppM.readSpagoManifest

  let
    newManifest = Map.unionWith unionFn manifest updates
    unionFn l r = case l, r of
      Right m1, Right m2 -> Right $ Map.union m1 m2
      Left x, _ -> Left x
      Right m1, _ -> Right m1

  AppM.writeSpagoManifest newManifest

  named <- AppM.readNamedManifest

  let
    named' :: Maybe NamedManifest
    named' = do
      let unstableChannel = ToolChannel { tool: Spago, channel: Unstable }
      let stableChannel = ToolChannel { tool: Spago, channel: Stable }

      ToolPackage { version: unstable } <- Map.lookup unstableChannel named
      ToolPackage { version: stable } <- Map.lookup stableChannel named

      let
        minSpaghetto = SemVer { version: fromRight' (\_ -> unsafeCrashWith "bad") (Version.parse "0.90.0"), pre: Nothing }

        allVersions = Map.keys newManifest
        allStableVersions = Set.filter (_ < minSpaghetto) allVersions

        maxUnstable = Set.findMax allVersions
        maxStable = Set.findMax allStableVersions

        insertPackage :: ToolChannel -> SemVer -> NamedManifest -> NamedManifest
        insertPackage channel version = Map.insert channel (ToolPackage { tool: Spago, version })

        updateUnstable :: NamedManifest -> NamedManifest
        updateUnstable prev = case maxUnstable of
          Just version | version > unstable -> insertPackage unstableChannel version prev
          _ -> prev

        updateStable :: NamedManifest -> NamedManifest
        updateStable prev = case maxStable of
          Just version | version > stable -> insertPackage stableChannel version prev
          _ -> prev

      pure (updateStable (updateUnstable named))

  case named' of
    Nothing -> Console.log "Failed to update named manifest" *> liftEffect (Process.exit 1)
    Just result -> AppM.writeNamedManifest result

prefetchPursTidy :: AppM PursTidyManifest
prefetchPursTidy = do
  manifest <- AppM.readPursTidyManifest

  let
    existing :: Set SemVer
    existing = Map.keys manifest

  rawReleases <- AppM.runGitHubM (GitHub.listReleases PursTidy) >>= case _ of
    Left error -> do
      Console.log $ "Failed to fetch releases for " <> Tool.print PursTidy Tool.Executable
      Console.log $ Octokit.printGitHubError error
      liftEffect $ Process.exit 1
    Right releases -> pure releases

  Console.log $ "Retrieved " <> show (Array.length rawReleases) <> " releases for " <> Tool.print PursTidy Tool.Executable

  let
    parsePursTidyReleases :: Array Release -> Set SemVer
    parsePursTidyReleases = Set.fromFoldable <<< Array.mapMaybe \release -> Either.hush do
      version <- SemVer.parse (fromMaybe release.tag (String.stripPrefix (String.Pattern "v") release.tag))
      pure version

    -- We only accept purs-tidy releases back to 0.5.0
    isBeforeCutoff :: SemVer -> Boolean
    isBeforeCutoff (SemVer { version }) =
      Version.major version == 0 && Version.minor version < 5

    supportedReleases :: Set SemVer
    supportedReleases = parsePursTidyReleases rawReleases # Set.filter (not <<< isBeforeCutoff)

    -- We only want to include releases that aren't already present in the
    -- manifest file.
    newReleases :: Set SemVer
    newReleases = Set.filter (not <<< flip Set.member existing) supportedReleases

  Console.log $ "Found " <> show (Set.size newReleases) <> " new releases for " <> Tool.print PursTidy Tool.Executable

  hashedReleases :: Array (Tuple SemVer FetchUrl) <- for (Set.toUnfoldable newReleases) \version -> do
    Console.log $ "Processing release " <> SemVer.print version
    Console.log $ "Fetching NPM tarball associated with version " <> SemVer.print version
    let url = "https://registry.npmjs.org/purs-tidy/-/purs-tidy-" <> SemVer.print version <> ".tgz"
    Nix.Prefetch.nixPrefetchTarball url >>= case _ of
      Left error -> do
        Console.log $ "Failed to hash NPM tarball at url " <> url <> ": " <> error
        liftEffect $ Process.exit 1
      Right hash -> pure $ Tuple version { url, hash }

  pure $ Map.fromFoldable hashedReleases

writePursTidyUpdates :: PursTidyManifest -> AppM Unit
writePursTidyUpdates updates = do
  manifest <- AppM.readPursTidyManifest
  let newManifest = Map.union manifest updates
  AppM.writePursTidyManifest newManifest

  named <- AppM.readNamedManifest

  let
    named' :: Maybe NamedManifest
    named' = do
      let unstableChannel = ToolChannel { tool: PursTidy, channel: Unstable }
      let stableChannel = ToolChannel { tool: PursTidy, channel: Stable }

      ToolPackage { version: unstable } <- Map.lookup unstableChannel named
      ToolPackage { version: stable } <- Map.lookup stableChannel named

      let
        allVersions = Map.keys newManifest
        allStableVersions = allVersions

        maxUnstable = Set.findMax allVersions
        maxStable = Set.findMax allStableVersions

        insertPackage :: ToolChannel -> SemVer -> NamedManifest -> NamedManifest
        insertPackage channel version = Map.insert channel (ToolPackage { tool: PursTidy, version })

        updateUnstable :: NamedManifest -> NamedManifest
        updateUnstable prev = case maxUnstable of
          Just version | version > unstable -> insertPackage unstableChannel version prev
          _ -> prev

        updateStable :: NamedManifest -> NamedManifest
        updateStable prev = case maxStable of
          Just version | version > stable -> insertPackage stableChannel version prev
          _ -> prev

      pure (updateStable (updateUnstable named))

  case named' of
    Nothing -> Console.log "Failed to update named manifest" *> liftEffect (Process.exit 1)
    Just result -> AppM.writeNamedManifest result
