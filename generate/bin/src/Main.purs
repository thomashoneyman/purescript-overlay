module Bin.Main where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Bin.AppM (AppM)
import Bin.AppM as AppM
import Control.Monad.Reader (ask)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (alaF)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Lib.Foreign.Octokit (Release, ReleaseAsset)
import Lib.Foreign.Octokit as Octokit
import Lib.Git (Tag(..))
import Lib.GitHub (Repo(..))
import Lib.GitHub as GitHub
import Lib.Nix.Manifest (Manifests, PursManifestEntry, SpagoManifest, PursManifest)
import Lib.Nix.Manifest as Nix.Manifest
import Lib.Nix.Prefetch as Nix.Prefetch
import Lib.Nix.System (NixSystem)
import Lib.Nix.System as Nix.System
import Lib.Nix.Version (NixVersion)
import Lib.Nix.Version as Nix.Version
import Lib.Utils as Utils
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Registry.Sha256 as Sha256

data Commit = DoCommit | NoCommit

derive instance Eq Commit

data Command
  = Verify FilePath
  | Prefetch FilePath
  | Update FilePath Commit

derive instance Eq Command

parser :: ArgParser Command
parser =
  Arg.choose "command"
    [ Arg.command [ "verify" ]
        "Verify that the generation script can read and write the manifests."
        do
          Verify <$> manifestDir
            <* Arg.flagHelp
    , Arg.command [ "prefetch" ]
        "Run the generation script without modifying files (print output)."
        do
          Prefetch <$> manifestDir
            <* Arg.flagHelp
    , Arg.command [ "update" ]
        "Run the generation script and write files."
        do
          Update
            <$> manifestDir
            <*> updateOptions
            <* Arg.flagHelp
    ] <* Arg.flagHelp
  where
  manifestDir =
    Arg.anyNotFlag "MANIFEST_DIR" "Location of the tooling manifests"

  updateOptions =
    Arg.flag [ "--commit" ]
      "Whether to commit results and open a pull request. Default: false"
      # Arg.boolean
      # Arg.default false
      # map (if _ then DoCommit else NoCommit)

main :: Effect Unit
main = Aff.launchAff_ do
  args <- Array.drop 2 <$> liftEffect Process.argv

  let description = "A generation script for updating PureScript tooling versions."
  mode <- case Arg.parseArgs "generate" description parser args of
    Left error -> do
      Console.log (Arg.printArgError error)
      case error of
        Arg.ArgError _ Arg.ShowHelp -> do
          liftEffect (Process.exit 0)
        _ ->
          liftEffect (Process.exit 1)
    Right command ->
      pure command

  octokit <- Octokit.newOctokit

  case mode of
    Verify dir -> AppM.runAppM { octokit, manifestDir: dir } do
      Console.log "Verifying manifests..."
      manifests <- readManifests
      let pursEntries = alaF Additive foldMap Map.size (Map.values manifests.purs)
      Console.log $ "Successfully parsed purs.json with " <> Int.toStringAs Int.decimal pursEntries <> " entries."
      let spagoEntries = Map.size manifests.spago
      Console.log $ "Successfully parsed spago.json with " <> Int.toStringAs Int.decimal spagoEntries <> " entries."

    Prefetch dir -> AppM.runAppM { octokit, manifestDir: dir } do
      Console.log "Prefetching new releases..."
      manifests <- readManifests
      updates <- fetchUpdates manifests
      if Map.size updates.purs > 0 then
        Console.log $ "New purs releases: " <> Utils.printJson Nix.Manifest.pursManifestCodec updates.purs
      else Console.log "No new purs releases."
      if Map.size updates.spago > 0 then
        Console.log $ "New spago releases: " <> Utils.printJson Nix.Manifest.spagoManifestCodec updates.spago
      else Console.log "No new spago releases."

    Update dir commit -> AppM.runAppM { octokit, manifestDir: dir } do
      case commit of
        DoCommit ->
          Console.log "Committing is not yet working. Updating locally only (not committing results)."
        -- committing results, and opening a pull request if necessary..."
        NoCommit ->
          Console.log "Updating locally only (not committing results)"

      manifests <- readManifests
      updates <- fetchUpdates manifests

      if Map.size updates.purs > 0 then do
        Console.log $ "New purs releases: " <> Utils.printJson Nix.Manifest.pursManifestCodec updates.purs
        Console.log "Writing to disk..."
        let merged = Map.unionWith Map.union manifests.purs updates.purs
        writePursManifest merged
      else do
        Console.log "No new purs releases."

      if Map.size updates.spago > 0 then do
        Console.log $ "New spago releases: " <> Utils.printJson Nix.Manifest.spagoManifestCodec updates.spago
        Console.log "Writing to disk..."
        let merged = Map.union manifests.spago updates.spago
        writeSpagoManifest merged
      else do
        Console.log "No new spago releases."

readManifests :: AppM Manifests
readManifests = do
  purs <- readPursManifest
  spago <- readSpagoManifest
  pure { purs, spago }

-- | Retrieve all relevant releases for the various supported tools from GitHub.
fetchUpdates :: Manifests -> AppM Manifests
fetchUpdates existing = do
  Console.log "Fetching releases..."
  purs <- fetchPursReleases (Set.unions $ map Map.keys $ Map.values existing.purs)
  Console.log $ "Fetched purs releases: " <> Utils.printJson Nix.Manifest.pursManifestCodec purs
  spago <- fetchSpagoReleases
  pure { purs, spago }

-- | Fetch all releases from the PureScript compiler repository and format them
-- | as Nix-compatible manifests.
fetchPursReleases :: Set NixVersion -> AppM PursManifest
fetchPursReleases existing = do
  Console.log "Fetching purs releases..."
  eitherReleases <- AppM.runGitHubM $ GitHub.listReleases PursRepo
  case eitherReleases of
    Left error -> do
      Console.log "Failed to fetch purs releases:"
      Console.log $ Octokit.printGitHubError error
      liftEffect (Process.exit 1)
    Right releases -> do
      manifests <- map Array.catMaybes $ traverse (pursReleaseToManifest existing) releases
      pure $ Array.foldl (Map.unionWith Map.union) Map.empty manifests

omittedPursReleases :: Array Tag
omittedPursReleases =
  [ Tag "v0.13.1" -- https://github.com/purescript/purescript/releases/tag/v0.13.1 (doesn't work)
  , Tag "v0.13.7" -- https://github.com/purescript/purescript/releases/tag/v0.13.7 (has no releases)
  , Tag "v0.15.1" -- https://github.com/purescript/purescript/releases/tag/v0.15.1 (incorrect version number, identical to 0.15.2)
  ]

isPre0_13 :: String -> Boolean
isPre0_13 input = do
  let trimmed = fromMaybe input (String.stripPrefix (String.Pattern "v") input)
  fromMaybe false do
    let array = String.split (String.Pattern ".") trimmed
    majorStr <- Array.index array 0
    minorStr <- Array.index array 1
    major <- Int.fromString majorStr
    minor <- Int.fromString minorStr
    pure $ major == 0 && minor < 13

isSupportedAsset :: ReleaseAsset -> Boolean
isSupportedAsset asset = do
  isJust (String.stripSuffix (String.Pattern ".tar.gz") asset.name)
    && not (String.contains (String.Pattern "win64") asset.name)

-- | Convert a release to a manifest, returning Nothing if the release ought to
-- | be skipped.
pursReleaseToManifest :: Set NixVersion -> Release -> AppM (Maybe PursManifest)
pursReleaseToManifest existing release = do
  Console.log $ "\nProcessing release " <> release.tag
  if isPre0_13 release.tag then do
    Console.log $ "Omitting release " <> release.tag <> " because it is prior to purs-0.13"
    pure Nothing
  else if Array.elem (Tag release.tag) omittedPursReleases then do
    Console.log $ "Omitting release " <> release.tag <> " because it is in the omitted purs releases list."
    pure Nothing
  else if release.draft then do
    Console.log $ "Omitting release " <> release.tag <> " because it is a draft."
    pure Nothing
  else case Nix.Version.parse $ fromMaybe release.tag $ String.stripPrefix (String.Pattern "v") release.tag of
    Left error -> do
      Console.log $ "Skipping release because it could not be parsed as a Nix version (X.Y.Z or X.Y.Z-N)."
      Console.log error
      pure Nothing
    Right version
      | Set.member version existing -> do
          Console.log $ "Skipping release because it already exists in the manifests file."
          pure Nothing
      | otherwise -> do
          let supported = Array.filter isSupportedAsset release.assets
          when (Array.length supported < 2) do
            Console.log "PureScript releases always have at least 2 supported release assets, but this release has fewer."
            Console.log $ Utils.printJson Octokit.releaseCodec release
            liftEffect (Process.exit 1)
          entries :: Array (Tuple NixSystem PursManifestEntry) <- for supported \tarball -> do
            system <- case Nix.System.fromPursReleaseTarball tarball.name of
              Left error -> do
                Console.log $ "Failed to parse tarball in release " <> release.tag <> " named " <> tarball.name
                Console.log error
                liftEffect (Process.exit 1)
              Right system -> pure system
            Console.log $ "Chose Nix system " <> Nix.System.print system <> " for tarball " <> tarball.name
            sha256 <- Nix.Prefetch.nixPrefetchTarball tarball.downloadUrl >>= case _ of
              Left error -> do
                Console.log $ "Could not prefetch hash for tarball at url " <> tarball.downloadUrl
                Console.log error
                liftEffect (Process.exit 1)
              Right sha256 -> pure sha256
            Console.log $ "Got hash " <> Sha256.print sha256 <> " for tarball " <> tarball.name
            pure (Tuple system { url: tarball.downloadUrl, hash: sha256 })
          let union prev (Tuple system entry) = Map.insertWith Map.union system (Map.singleton version entry) prev
          pure $ Just $ Array.foldl union Map.empty entries

-- TODO: Spago's alpha does not currently have any official releases, so we
-- don't fetch anything.
fetchSpagoReleases :: AppM SpagoManifest
fetchSpagoReleases = do
  Console.log "Fetching spago releases..."
  pure Map.empty

getPursManifestPath :: AppM FilePath
getPursManifestPath = do
  { manifestDir } <- ask
  pure $ Path.concat [ manifestDir, "purs.json" ]

getSpagoManifestPath :: AppM FilePath
getSpagoManifestPath = do
  { manifestDir } <- ask
  pure $ Path.concat [ manifestDir, "spago.json" ]

readPursManifest :: AppM PursManifest
readPursManifest = do
  path <- getPursManifestPath
  Utils.readJsonFile path Nix.Manifest.pursManifestCodec

writePursManifest :: PursManifest -> AppM Unit
writePursManifest manifest = do
  path <- getPursManifestPath
  Utils.writeJsonFile path Nix.Manifest.pursManifestCodec manifest

readSpagoManifest :: AppM SpagoManifest
readSpagoManifest = do
  path <- getSpagoManifestPath
  Utils.readJsonFile path Nix.Manifest.spagoManifestCodec

writeSpagoManifest :: SpagoManifest -> AppM Unit
writeSpagoManifest manifest = do
  path <- getPursManifestPath
  Utils.writeJsonFile path Nix.Manifest.spagoManifestCodec manifest
