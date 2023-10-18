module Bin.Run
  ( prefetchPurs
  , prefetchPursBackendEs
  , prefetchPursLanguageServer
  , prefetchPursTidy
  , prefetchSpago
  , verifyPurs
  , verifyPursBackendEs
  , verifyPursLanguageServer
  , verifyPursTidy
  , verifySpago
  , writePursBackendEsUpdates
  , writePursLanguageServerUpdates
  , writePursTidyUpdates
  , writePursUpdates
  , writeSpagoUpdates
  ) where

import Prelude

import Bin.AppM (AppM)
import Bin.AppM as AppM
import Control.Alt ((<|>))
import Control.Alternative (guard)
import Data.Array as Array
import Data.Either (Either(..), fromRight')
import Data.Either as Either
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (alaF)
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
import Lib.GitHub as GitHub
import Lib.Nix.Manifest (CombinedManifest, FetchUrl, GitHubBinaryManifest, NamedManifest, NPMRegistryManifest)
import Lib.Nix.Prefetch as Nix.Prefetch
import Lib.Nix.System (NixSystem(..))
import Lib.Nix.System as NixSystem
import Lib.SemVer (SemVer(..))
import Lib.SemVer as SemVer
import Lib.Tool (Channel(..), Tool(..), ToolChannel(..), ToolPackage(..))
import Lib.Tool as Tool
import Node.Process as Process
import Partial.Unsafe (unsafeCrashWith)
import Registry.Version (Version)
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
  Console.log $ "Successfully parsed spago.json with " <> Int.toStringAs Int.decimal entries <> " entries."

verifyPursTidy :: AppM Unit
verifyPursTidy = do
  manifest <- AppM.readPursTidyManifest
  let entries = Map.size manifest
  Console.log $ "Successfully parsed purs-tidy.json with " <> Int.toStringAs Int.decimal entries <> " entries."

verifyPursBackendEs :: AppM Unit
verifyPursBackendEs = do
  manifest <- AppM.readPursBackendEsManifest
  let entries = Map.size manifest
  Console.log $ "Successfully parsed purs-backend-es.json with " <> Int.toStringAs Int.decimal entries <> " entries."

verifyPursLanguageServer :: AppM Unit
verifyPursLanguageServer = do
  manifest <- AppM.readPursLanguageServerManifest
  let entries = Map.size manifest
  Console.log $ "Successfully parsed purescript-language-server.json with " <> Int.toStringAs Int.decimal entries <> " entries."

prefetchPurs :: AppM GitHubBinaryManifest
prefetchPurs = fetchGitHub
  { tool: Purs
  , readManifest: AppM.readPursManifest

  , parseAsset: \asset -> do
      guard $ isJust $ String.stripSuffix (String.Pattern ".tar.gz") asset.name
      guard $ not $ String.contains (String.Pattern "win64") asset.name
      system <- Either.hush $ NixSystem.fromPursReleaseTarball asset.name
      pure $ Tuple system asset.downloadUrl

  , filterVersion: \(SemVer { version }) -> do
      let
        -- We only accept compiler releases back to 0.13
        afterCutoff :: Boolean
        afterCutoff = Version.major version == 0 && Version.minor version >= 13

        -- Some versions are broken and we don't want to include them
        notBroken :: Boolean
        notBroken = Array.notElem version
          [ unsafeVersion "0.13.1" -- https://github.com/purescript/purescript/releases/tag/v0.13.1 (doesn't work)
          , unsafeVersion "0.13.7" -- https://github.com/purescript/purescript/releases/tag/v0.13.7 (has no releases)
          , unsafeVersion "0.15.1" -- https://github.com/purescript/purescript/releases/tag/v0.15.1 (incorrect version number, identical to 0.15.2)
          ]

      afterCutoff && notBroken

  , filterSystem: \(SemVer { version, pre }) system ->
      -- Darwin prereleases from 0.15.0 to 0.15.9 have incorrect version numbers
      if system == X86_64_darwin && Version.major version == 0 && Version.minor version == 15 then case pre of
        Nothing -> false
        Just _ -> Version.patch version < 10
      else
        false
  }

writePursUpdates :: GitHubBinaryManifest -> AppM Unit
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

-- Spago was rewritten from Haskell to PureScript, so we only get the Haskell
-- releases from GitHub (pre-0.90).
prefetchSpagoGitHub :: AppM GitHubBinaryManifest
prefetchSpagoGitHub = fetchGitHub
  { tool: Spago
  , readManifest: map (Map.mapMaybeWithKey (const Either.hush)) AppM.readSpagoManifest

  , parseAsset: \asset -> do
      trimmed <- String.stripSuffix (String.Pattern ".tar.gz") asset.name
      guard $ not $ Array.elem trimmed [ "Windows", "windows" ]
      system <- Either.hush $ NixSystem.fromSpagoReleaseTarball asset.name
      pure $ Tuple system asset.downloadUrl

  , filterVersion: \(SemVer { version, pre }) -> do
      let
        -- We only accept Spago releases back to 0.18, which is the first one with
        -- a configurable cache (so we don't get Nix errors trying to run Spago)
        satisfiesLowerLimit :: Boolean
        satisfiesLowerLimit = Version.major version == 0 && Version.minor version >= 18

        -- We only accept Spago releases up to 0.90, which is the first release
        -- of spago-next
        satisfiesUpperLimit :: Boolean
        satisfiesUpperLimit = Version.major version == 0 && Version.minor version <= 90

        -- Some versions are broken and we don't want to include them
        notBroken :: Boolean
        notBroken = Array.notElem version
          [ unsafeVersion "0.19.2" -- wrong version number
          ]

        -- No prereleases for legacy spago
        notPrerelease :: Boolean
        notPrerelease = isNothing pre

      satisfiesLowerLimit && satisfiesUpperLimit && notBroken && notPrerelease

  , filterSystem: \_ _ -> true
  }

prefetchSpago :: AppM CombinedManifest
prefetchSpago = do
  github <- prefetchSpagoGitHub
  -- FIXME!!!
  -- npm <- prefetchSpagoNPM
  -- pure $ Map.union (map Left npm) (map Right github)
  pure (map Right github)

writeSpagoUpdates :: CombinedManifest -> AppM Unit
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

prefetchPursTidy :: AppM NPMRegistryManifest
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

writePursTidyUpdates :: NPMRegistryManifest -> AppM Unit
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

prefetchPursBackendEs :: AppM NPMRegistryManifest
prefetchPursBackendEs = do
  manifest <- AppM.readPursBackendEsManifest

  let
    existing :: Set SemVer
    existing = Map.keys manifest

  rawReleases <- AppM.runGitHubM (GitHub.listReleases PursBackendEs) >>= case _ of
    Left error -> do
      Console.log $ "Failed to fetch releases for " <> Tool.print PursBackendEs Tool.Executable
      Console.log $ Octokit.printGitHubError error
      liftEffect $ Process.exit 1
    Right releases -> pure releases

  Console.log $ "Retrieved " <> show (Array.length rawReleases) <> " releases for " <> Tool.print PursBackendEs Tool.Executable

  let
    parsePursBackendEsReleases :: Array Release -> Set SemVer
    parsePursBackendEsReleases = Set.fromFoldable <<< Array.mapMaybe \release -> Either.hush do
      version <- SemVer.parse $ fromMaybe release.tag do
        String.stripPrefix (String.Pattern "v") release.tag
          <|> String.stripPrefix (String.Pattern "purs-backend-es-v") release.tag
      pure version

    supportedReleases :: Set SemVer
    supportedReleases = parsePursBackendEsReleases rawReleases

    -- We only want to include releases that aren't already present in the
    -- manifest file.
    newReleases :: Set SemVer
    newReleases = Set.filter (not <<< flip Set.member existing) supportedReleases

  Console.log $ "Found " <> show (Set.size newReleases) <> " new releases for " <> Tool.print PursBackendEs Tool.Executable

  hashedReleases :: Array (Tuple SemVer FetchUrl) <- for (Set.toUnfoldable newReleases) \version -> do
    Console.log $ "Processing release " <> SemVer.print version
    Console.log $ "Fetching NPM tarball associated with version " <> SemVer.print version
    let url = "https://registry.npmjs.org/purs-backend-es/-/purs-backend-es-" <> SemVer.print version <> ".tgz"
    Nix.Prefetch.nixPrefetchTarball url >>= case _ of
      Left error -> do
        Console.log $ "Failed to hash NPM tarball at url " <> url <> ": " <> error
        liftEffect $ Process.exit 1
      Right hash -> pure $ Tuple version { url, hash }

  pure $ Map.fromFoldable hashedReleases

writePursBackendEsUpdates :: NPMRegistryManifest -> AppM Unit
writePursBackendEsUpdates updates = do
  manifest <- AppM.readPursBackendEsManifest
  let newManifest = Map.union manifest updates
  AppM.writePursBackendEsManifest newManifest

  named <- AppM.readNamedManifest

  let
    named' :: Maybe NamedManifest
    named' = do
      let unstableChannel = ToolChannel { tool: PursBackendEs, channel: Unstable }
      let stableChannel = ToolChannel { tool: PursBackendEs, channel: Stable }

      ToolPackage { version: unstable } <- Map.lookup unstableChannel named
      ToolPackage { version: stable } <- Map.lookup stableChannel named

      let
        allVersions = Map.keys newManifest
        allStableVersions = allVersions

        maxUnstable = Set.findMax allVersions
        maxStable = Set.findMax allStableVersions

        insertPackage :: ToolChannel -> SemVer -> NamedManifest -> NamedManifest
        insertPackage channel version = Map.insert channel (ToolPackage { tool: PursBackendEs, version })

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

prefetchPursLanguageServer :: AppM NPMRegistryManifest
prefetchPursLanguageServer = do
  manifest <- AppM.readPursLanguageServerManifest

  let
    existing :: Set SemVer
    existing = Map.keys manifest

  rawReleases <- AppM.runGitHubM (GitHub.listReleases PursLanguageServer) >>= case _ of
    Left error -> do
      Console.log $ "Failed to fetch releases for " <> Tool.print PursLanguageServer Tool.Executable
      Console.log $ Octokit.printGitHubError error
      liftEffect $ Process.exit 1
    Right releases -> pure releases

  Console.log $ "Retrieved " <> show (Array.length rawReleases) <> " releases for " <> Tool.print PursLanguageServer Tool.Executable

  let
    parsePursLanguageServerReleases :: Array Release -> Map SemVer String
    parsePursLanguageServerReleases = Map.fromFoldable <<< Array.mapMaybe \release -> Either.hush do
      version <- SemVer.parse $ fromMaybe release.tag $ String.stripPrefix (String.Pattern "v") release.tag
      asset <- Either.note "No asset named 'purescript-language-server.js' in release." $ Array.find (\asset -> asset.name == "purescript-language-server.js") release.assets
      pure $ Tuple version asset.downloadUrl

    -- We only accept pre-bundled language server releases, ie. those after 0.15.5
    isBeforeCutoff :: SemVer -> Boolean
    isBeforeCutoff (SemVer { version }) = Version.major version == 0 && Version.minor version <= 15 && Version.patch version <= 5

    supportedReleases :: Map SemVer String
    supportedReleases = Map.filterKeys (not <<< isBeforeCutoff) $ parsePursLanguageServerReleases rawReleases

    -- We only want to include releases that aren't already present in the
    -- manifest file.
    newReleases :: Map SemVer String
    newReleases = Map.filterKeys (not <<< flip Set.member existing) supportedReleases

  Console.log $ "Found " <> show (Map.size newReleases) <> " new releases for " <> Tool.print PursLanguageServer Tool.Executable

  hashedReleases <- forWithIndex newReleases \version downloadUrl -> do
    Console.log $ "Processing release " <> SemVer.print version
    Console.log $ "Fetching hashes for release asset download url " <> downloadUrl
    Nix.Prefetch.nixPrefetchTarball downloadUrl >>= case _ of
      Left error -> do
        Console.log $ "Failed to hash release asset at url " <> downloadUrl <> ": " <> error
        liftEffect $ Process.exit 1
      Right hash -> pure { url: downloadUrl, hash }

  pure hashedReleases

writePursLanguageServerUpdates :: NPMRegistryManifest -> AppM Unit
writePursLanguageServerUpdates updates = do
  manifest <- AppM.readPursLanguageServerManifest
  let newManifest = Map.union manifest updates
  AppM.writePursLanguageServerManifest newManifest

  named <- AppM.readNamedManifest

  let
    named' :: Maybe NamedManifest
    named' = do
      let unstableChannel = ToolChannel { tool: PursLanguageServer, channel: Unstable }
      let stableChannel = ToolChannel { tool: PursLanguageServer, channel: Stable }

      ToolPackage { version: unstable } <- Map.lookup unstableChannel named
      ToolPackage { version: stable } <- Map.lookup stableChannel named

      let
        allVersions = Map.keys newManifest
        allStableVersions = allVersions

        maxUnstable = Set.findMax allVersions
        maxStable = Set.findMax allStableVersions

        insertPackage :: ToolChannel -> SemVer -> NamedManifest -> NamedManifest
        insertPackage channel version = Map.insert channel (ToolPackage { tool: PursLanguageServer, version })

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

unsafeVersion :: String -> Version
unsafeVersion = Either.fromRight' (\_ -> unsafeCrashWith "Unexpected Left") <<< Version.parse

type GitHubArgs =
  { tool :: Tool
  , readManifest :: AppM (Map SemVer (Map NixSystem FetchUrl))
  , parseAsset :: ReleaseAsset -> Maybe (Tuple NixSystem String)
  , filterVersion :: SemVer -> Boolean
  , filterSystem :: SemVer -> NixSystem -> Boolean
  }

fetchGitHub :: GitHubArgs -> AppM (Map SemVer (Map NixSystem FetchUrl))
fetchGitHub { tool, readManifest, parseAsset, filterVersion, filterSystem } = do
  manifest <- readManifest

  let existingReleases = Map.keys manifest

  rawReleases <- AppM.runGitHubM (GitHub.listReleases tool) >>= case _ of
    Left error -> do
      Console.log $ "Failed to fetch releases for " <> Tool.print tool Tool.Executable
      Console.log $ "  due to a GitHub error: "
      Console.log $ Octokit.printGitHubError error
      liftEffect $ Process.exit 1
    Right releases -> pure releases

  Console.log $ "Retrieved " <> show (Array.length rawReleases) <> " releases in total for " <> Tool.print tool Tool.Executable

  let
    parseReleases :: Array Release -> Map SemVer (Map NixSystem String)
    parseReleases = Map.fromFoldable <<< Array.mapMaybe \release -> Either.hush do
      version <- SemVer.parse (fromMaybe release.tag (String.stripPrefix (String.Pattern "v") release.tag))
      let supportedAssets = Map.fromFoldable $ Array.mapMaybe parseAsset release.assets
      pure $ Tuple version supportedAssets

    supportedReleases :: Map SemVer (Map NixSystem String)
    supportedReleases = parseReleases rawReleases # Map.mapMaybeWithKey \version assets -> do
      guard $ not $ filterVersion version
      pure $ Map.filterKeys (not <<< filterSystem version) assets

    -- We only want to include releases that aren't already present in the
    -- manifest file.
    newReleases :: Map SemVer (Map NixSystem String)
    newReleases = Map.filterKeys (not <<< flip Set.member existingReleases) supportedReleases

  Console.log $ "Found " <> show (Map.size newReleases) <> " new releases for " <> Tool.print tool Tool.Executable

  hashedReleases <- forWithIndex newReleases \version assets -> do
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
