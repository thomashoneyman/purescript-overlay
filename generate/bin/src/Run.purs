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
import Control.Alternative (guard)
import Control.Monad.Reader (ask)
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
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Lib.Foreign.Octokit (Release, ReleaseAsset)
import Lib.Foreign.Octokit as Octokit
import Lib.Foreign.Tmp as Tmp
import Lib.GitHub as GitHub
import Lib.NPMRegistry (NPMVersion)
import Lib.NPMRegistry as NPMRegistry
import Lib.Nix.Manifest (CombinedManifest, FetchUrl, GitHubBinaryManifest, ManifestCodec, NPMFetch(..), NPMRegistryManifest, NamedManifest)
import Lib.Nix.Manifest as Manifest
import Lib.Nix.Prefetch as Nix.Prefetch
import Lib.Nix.System (NixSystem(..))
import Lib.Nix.System as NixSystem
import Lib.SemVer (SemVer(..))
import Lib.SemVer as SemVer
import Lib.Tool (Channel(..), Tool(..), ToolChannel(..), ToolPackage(..))
import Lib.Tool as Tool
import Lib.Utils (die, withBackoff')
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS.Aff
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Partial.Unsafe (unsafeCrashWith)
import Registry.Sha256 as Sha256
import Registry.Version (Version)
import Registry.Version as Version

-- | Generic verify function that reads a manifest and reports entry count
verify :: forall a. ManifestCodec a -> (a -> Int) -> AppM Unit
verify manifestCodec countEntries = do
  manifest <- AppM.readManifest manifestCodec
  let entries = countEntries manifest
  Console.log $ "Successfully parsed " <> Manifest.filename manifestCodec.tool
    <> " with "
    <> Int.toStringAs Int.decimal entries
    <> " entries."

verifyPurs :: AppM Unit
verifyPurs = verify Manifest.pursManifest (alaF Additive foldMap Map.size <<< Map.values)

verifySpago :: AppM Unit
verifySpago = verify Manifest.spagoManifest Map.size

verifyPursTidy :: AppM Unit
verifyPursTidy = verify Manifest.pursTidyManifest Map.size

verifyPursBackendEs :: AppM Unit
verifyPursBackendEs = verify Manifest.pursBackendEsManifest Map.size

verifyPursLanguageServer :: AppM Unit
verifyPursLanguageServer = verify Manifest.pursLanguageServerManifest Map.size

prefetchPurs :: AppM GitHubBinaryManifest
prefetchPurs = fetchGitHub
  { tool: Purs
  , readManifest: AppM.readManifest Manifest.pursManifest

  , parseAsset: \asset -> do
      guard $ isJust $ String.stripSuffix (String.Pattern ".tar.gz") asset.name
      guard $ not $ String.contains (String.Pattern "win64") asset.name
      system <- Either.hush $ NixSystem.fromPursReleaseTarball asset.name
      pure $ Tuple system asset.downloadUrl

  , filterVersion: \(SemVer { version }) -> do
      let
        -- We only accept compiler releases back to 0.13.0
        afterCutoff :: Boolean
        afterCutoff = version >= unsafeVersion "0.13.0"

        -- Some versions are broken and we don't want to include them
        broken :: Boolean
        broken = Array.elem version
          [ unsafeVersion "0.13.1" -- https://github.com/purescript/purescript/releases/tag/v0.13.1 (doesn't work)
          , unsafeVersion "0.13.7" -- https://github.com/purescript/purescript/releases/tag/v0.13.7 (has no releases)
          , unsafeVersion "0.15.1" -- https://github.com/purescript/purescript/releases/tag/v0.15.1 (incorrect version number, identical to 0.15.2)
          ]

      afterCutoff && not broken

  , filterSystem: \(SemVer { version, pre }) system ->
      -- Darwin prereleases from 0.15.0 to 0.15.10 have incorrect version numbers
      if system == X86_64_darwin && version >= unsafeVersion "0.15.0" && version <= unsafeVersion "0.15.10" then case pre of
        Nothing -> true
        Just _ -> false
      else
        true
  }

prefetchSpago :: AppM CombinedManifest
prefetchSpago = do
  github <- prefetchSpagoGitHub
  npm <- prefetchSpagoNPMRegistry
  pure $ Map.union (map Left npm) (map Right github)
  where
  -- Spago was rewritten from Haskell to PureScript, so we only get the PureScript
  -- releases from NPM (from 0.90 onward).
  prefetchSpagoNPMRegistry :: AppM NPMRegistryManifest
  prefetchSpagoNPMRegistry = prefetchNPMRegistry
    { tool: Spago
    , readManifest: map (Map.mapMaybeWithKey (const Either.blush)) (AppM.readManifest Manifest.spagoManifest)

    , includePackageLock: \_ -> Just Remote

    , filterVersion: \(SemVer { version, pre }) -> do
        let
          -- We only accept Spago releases from 0.93.19 onward (when both shell
          -- completions and proper package-lock.json were available). Earlier
          -- 0.93.x versions used a simple tarball format or had malformed
          -- lockfiles that we no longer support.
          satisfiesLowerLimit :: Boolean
          satisfiesLowerLimit = version >= unsafeVersion "0.93.19"

          notPrerelease :: Boolean
          notPrerelease = isNothing pre

        satisfiesLowerLimit && notPrerelease
    }

  -- Spago was rewritten from Haskell to PureScript, so we only get the Haskell
  -- releases from GitHub (pre-0.90).
  prefetchSpagoGitHub :: AppM GitHubBinaryManifest
  prefetchSpagoGitHub = fetchGitHub
    { tool: Spago
    , readManifest: map (Map.mapMaybeWithKey (const Either.hush)) (AppM.readManifest Manifest.spagoManifest)

    , parseAsset: \asset -> do
        trimmed <- String.stripSuffix (String.Pattern ".tar.gz") asset.name
        guard $ not $ Array.elem trimmed [ "Windows", "windows" ]
        system <- Either.hush $ NixSystem.fromSpagoReleaseTarball asset.name
        pure $ Tuple system asset.downloadUrl

    , filterVersion: \(SemVer { version, pre }) -> do
        let
          -- We only accept Spago releases from 0.18.0 to 0.21.0 (legacy Haskell spago).
          -- 0.18.0 is the first one with a configurable cache (so we don't get Nix errors).
          -- Versions after 0.21.0 are spago-next (PureScript rewrite) handled by NPM.
          inRange :: Boolean
          inRange = version >= unsafeVersion "0.18.0" && version <= unsafeVersion "0.21.0"

          -- Some versions are broken and we don't want to include them
          notBroken :: Boolean
          notBroken = Array.notElem version
            [ unsafeVersion "0.19.2" -- wrong version number
            ]

          -- No prereleases for legacy spago
          notPrerelease :: Boolean
          notPrerelease = isNothing pre

        inRange && notBroken && notPrerelease

    , filterSystem: \_ _ -> true
    }

prefetchPursTidy :: AppM NPMRegistryManifest
prefetchPursTidy = prefetchNPMRegistry
  { tool: PursTidy
  , readManifest: AppM.readManifest Manifest.pursTidyManifest
  , includePackageLock: \_ -> Nothing
  , filterVersion: \(SemVer { version }) -> version >= unsafeVersion "0.5.0"
  }

prefetchPursBackendEs :: AppM NPMRegistryManifest
prefetchPursBackendEs = prefetchNPMRegistry
  { tool: PursBackendEs
  , readManifest: AppM.readManifest Manifest.pursBackendEsManifest
  , includePackageLock: \_ -> Nothing
  , filterVersion: \_ -> true
  }

-- The language server is a bit weird because we fetch pre-bundled results from
-- GitHub instead of from NPM (where they aren't bundled), so this is a one-off
-- one instead of using the prefetch helpers.
prefetchPursLanguageServer :: AppM NPMRegistryManifest
prefetchPursLanguageServer = do
  manifest <- AppM.readManifest Manifest.pursLanguageServerManifest

  let existing = Map.keys manifest

  rawReleases <- AppM.runGitHubM (GitHub.listReleases PursLanguageServer) >>= case _ of
    Left error -> do
      die $ "Failed to fetch releases for purescript-language-server\n" <> Octokit.printGitHubError error
    Right releases -> pure releases

  Console.log $ "Retrieved " <> show (Array.length rawReleases) <> " releases for purescript-language-server"

  let
    parsePursLanguageServerReleases :: Array Release -> Map SemVer String
    parsePursLanguageServerReleases = Map.fromFoldable <<< Array.mapMaybe \release -> Either.hush do
      version <- SemVer.parse $ fromMaybe release.tag $ String.stripPrefix (String.Pattern "v") release.tag
      asset <- Either.note "No asset named 'purescript-language-server.js' in release." $ Array.find (\asset -> asset.name == "purescript-language-server.js") release.assets
      pure $ Tuple version asset.downloadUrl

    -- We only accept pre-bundled language server releases, ie. those after 0.15.5
    isBeforeCutoff :: SemVer -> Boolean
    isBeforeCutoff (SemVer { version }) = version <= unsafeVersion "0.15.5"

    supportedReleases :: Map SemVer String
    supportedReleases = Map.filterKeys (not <<< isBeforeCutoff) $ parsePursLanguageServerReleases rawReleases

    -- We only want to include releases that aren't already present in the
    -- manifest file.
    newReleases :: Map SemVer String
    newReleases = Map.filterKeys (not <<< flip Set.member existing) supportedReleases

  Console.log $ "Found " <> show (Map.size newReleases) <> " new releases for purescript-language-server"

  hashedReleases <- forWithIndex newReleases \version downloadUrl -> do
    Console.log $ "Processing release " <> SemVer.print version
    Console.log $ "Fetching hashes for release asset download url " <> downloadUrl
    Nix.Prefetch.nixPrefetchUrl downloadUrl >>= case _ of
      Left error -> do
        die $ "Failed to hash release asset at url " <> downloadUrl <> ": " <> error
      Right hash -> pure $ Bundled { url: downloadUrl, hash }

  pure hashedReleases

-- | Update the named manifest (stable/unstable channel pointers) for a tool
-- | given the set of all versions and a function to determine which are stable.
updateNamedManifest :: Tool -> Set SemVer -> (Set SemVer -> Set SemVer) -> AppM Unit
updateNamedManifest tool allVersions getStableVersions = do
  named <- AppM.readNamedManifest
  let
    unstableChannel = ToolChannel { tool, channel: Unstable }
    stableChannel = ToolChannel { tool, channel: Stable }

    named' :: Maybe NamedManifest
    named' = do
      ToolPackage { version: unstable } <- Map.lookup unstableChannel named
      ToolPackage { version: stable } <- Map.lookup stableChannel named

      let
        allStableVersions = getStableVersions allVersions
        maxUnstable = Set.findMax allVersions
        maxStable = Set.findMax allStableVersions

        updateChannel :: ToolChannel -> SemVer -> Maybe SemVer -> NamedManifest -> NamedManifest
        updateChannel channel current newMax prev = case newMax of
          Just version | version > current -> Map.insert channel (ToolPackage { tool, version }) prev
          _ -> prev

      pure $ updateChannel unstableChannel unstable maxUnstable
        $ updateChannel stableChannel stable maxStable
        $ named

  case named' of
    Nothing -> die "Failed to update named manifest"
    Just result -> AppM.writeNamedManifest result

writePursUpdates :: GitHubBinaryManifest -> AppM Unit
writePursUpdates updates = do
  manifest <- AppM.readManifest Manifest.pursManifest
  let newManifest = Map.unionWith Map.union manifest updates
  AppM.writeManifest Manifest.pursManifest newManifest
  updateNamedManifest Purs (Map.keys newManifest) $
    Set.filter (\(SemVer { pre }) -> isNothing pre)

writeSpagoUpdates :: CombinedManifest -> AppM Unit
writeSpagoUpdates updates = do
  manifest <- AppM.readManifest Manifest.spagoManifest
  let
    newManifest = Map.unionWith unionFn manifest updates
    unionFn l r = case l, r of
      Right m1, Right m2 -> Right $ Map.union m1 m2
      Left x, _ -> Left x
      Right m1, _ -> Right m1
    -- For spago, "stable" means pre-spaghetto (< 0.90.0)
    minSpaghetto = SemVer { version: fromRight' (\_ -> unsafeCrashWith "bad") (Version.parse "0.90.0"), pre: Nothing }
  AppM.writeManifest Manifest.spagoManifest newManifest
  updateNamedManifest Spago (Map.keys newManifest) $
    Set.filter (_ < minSpaghetto)

writePursTidyUpdates :: NPMRegistryManifest -> AppM Unit
writePursTidyUpdates updates = do
  manifest <- AppM.readManifest Manifest.pursTidyManifest
  let newManifest = Map.union manifest updates
  AppM.writeManifest Manifest.pursTidyManifest newManifest
  updateNamedManifest PursTidy (Map.keys newManifest) identity

writePursBackendEsUpdates :: NPMRegistryManifest -> AppM Unit
writePursBackendEsUpdates updates = do
  manifest <- AppM.readManifest Manifest.pursBackendEsManifest
  let newManifest = Map.union manifest updates
  AppM.writeManifest Manifest.pursBackendEsManifest newManifest
  updateNamedManifest PursBackendEs (Map.keys newManifest) identity

writePursLanguageServerUpdates :: NPMRegistryManifest -> AppM Unit
writePursLanguageServerUpdates updates = do
  manifest <- AppM.readManifest Manifest.pursLanguageServerManifest
  let newManifest = Map.union manifest updates
  AppM.writeManifest Manifest.pursLanguageServerManifest newManifest
  updateNamedManifest PursLanguageServer (Map.keys newManifest) identity

unsafeVersion :: String -> Version
unsafeVersion = Either.fromRight' (\_ -> unsafeCrashWith "Unexpected Left") <<< Version.parse

data PackageLockSource = Remote | Local FilePath

derive instance Eq PackageLockSource

type NPMRegistryArgs =
  { tool :: Tool
  , readManifest :: AppM NPMRegistryManifest
  , includePackageLock :: SemVer -> Maybe PackageLockSource
  , filterVersion :: SemVer -> Boolean
  }

prefetchNPMRegistry :: NPMRegistryArgs -> AppM NPMRegistryManifest
prefetchNPMRegistry { tool, readManifest, filterVersion, includePackageLock } = do
  manifest <- readManifest

  let existing = Map.keys manifest

  rawReleases <- liftAff (NPMRegistry.listNPMReleases tool) >>= case _ of
    Left error -> do
      die $ "Failed to fetch releases from NPM registry for " <> Tool.print tool Tool.Executable <> " at url " <> NPMRegistry.printNPMRegistryUrl tool <> "\n" <> error
    Right releases -> pure releases

  Console.log $ "Retrieved " <> show (Map.size rawReleases) <> " releases for " <> Tool.print tool Tool.Executable

  let
    supportedReleases :: Map SemVer NPMVersion
    supportedReleases = Map.filterKeys filterVersion rawReleases

    newReleases :: Map SemVer NPMVersion
    newReleases = Map.filterKeys (not <<< flip Set.member existing) supportedReleases

  Console.log $ "Found " <> show (Map.size newReleases) <> " new releases for " <> Tool.print tool Tool.Executable

  forWithIndex newReleases \version { gitHead: mbGitHead, dist } -> do
    Console.log $ "Processing release " <> SemVer.print version

    Console.log $ "Fetching hash for NPM tarball at url " <> dist.tarball
    tarballHash <- Nix.Prefetch.nixPrefetchUrl dist.tarball >>= case _ of
      Left error -> do
        die $ "Failed to hash tarball at url " <> dist.tarball <> ": " <> error
      Right hash -> pure { url: dist.tarball, hash }

    case includePackageLock version of
      Nothing -> do
        Console.log $ "Version is bundled, returning tarball hash..."
        pure $ Bundled tarballHash

      Just (Local path) -> do
        Console.log "Version is not bundled, fetching local lockfile..."
        { manifestDir } <- ask
        npmDepsHash <- withBackoff' (Nix.Prefetch.prefetchNpmDeps (Path.concat [ manifestDir, "build-support", path ])) >>= case _ of
          Nothing -> do
            die "Timed out trying to prefetch package-lock.json file."
          Just (Left error) -> do
            die $ "Failed to prefetch npm deps from local package-lock.json file " <> path <> "\n" <> error
          Just (Right hash) -> do
            Console.log $ "Got hash of npm deps from local package-lock.json file."
            Console.log $ Sha256.print hash
            pure hash
        pure $ UnbundledLocal { tarball: tarballHash, path, depsHash: npmDepsHash }

      Just Remote -> do
        Console.log "Version is not bundled, fetching lockfile..."
        { lockfileUrl, lockfileHash, npmDepsHash } <- case mbGitHead of
          Nothing -> do
            die "No git commit associated with version, cannot fetch package-lock.json."
          Just sha -> do
            let packageLockUrl = GitHub.rawContentUrl tool sha "package-lock.json"
            packageLockRemoteHash <- withBackoff' (Nix.Prefetch.nixPrefetchUrl packageLockUrl) >>= case _ of
              Nothing -> do
                die "Timed out trying to prefetch package-lock.json file."
              Just (Left error) -> do
                die $ "Failed to prefetch package-lock.json file from url " <> packageLockUrl <> "\n" <> error
              Just (Right hash) ->
                pure hash
            packageLockContents <- AppM.runGitHubM (GitHub.getContent tool sha "package-lock.json") >>= case _ of
              Left error -> do
                die $ "Failed to fetch remote package-lock.json file!\n" <> Octokit.printGitHubError error
              Right result ->
                pure result
            tmp <- Tmp.mkTmpDir
            let packageLockPath = Path.concat [ tmp, "package-lock.json" ]
            liftAff $ FS.Aff.writeTextFile UTF8 packageLockPath packageLockContents
            npmDepsHash <- Nix.Prefetch.prefetchNpmDeps packageLockPath >>= case _ of
              Left error -> do
                Console.log "Failed to prefetch npm deps from package-lock.json file"
                Console.log error
                liftAff $ FS.Aff.rm' tmp { force: true, maxRetries: 1, recursive: true, retryDelay: 100 }
                liftEffect $ Process.exit' 1
              Right hash -> do
                Console.log $ "Got hash of npm deps: " <> Sha256.print hash
                liftAff $ FS.Aff.rm' tmp { force: true, maxRetries: 1, recursive: true, retryDelay: 100 }
                pure hash
            pure { lockfileUrl: packageLockUrl, lockfileHash: packageLockRemoteHash, npmDepsHash }
        pure $ Unbundled { tarball: tarballHash, lockfile: { url: lockfileUrl, hash: lockfileHash }, depsHash: npmDepsHash }

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
      die $ String.joinWith "\n"
        [ "Failed to fetch releases for " <> Tool.print tool Tool.Executable
        , "  due to a GitHub error: "
        , Octokit.printGitHubError error
        ]
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
      guard $ filterVersion version
      pure $ Map.filterKeys (filterSystem version) assets

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
      Nix.Prefetch.nixPrefetchUrl asset >>= case _ of
        Left error -> do
          die $ "Failed to hash release asset at url " <> asset <> ": " <> error
        Right hash -> pure { url: asset, hash }

  pure hashedReleases
