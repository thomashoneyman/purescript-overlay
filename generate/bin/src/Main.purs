module Bin.Main where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Bin.AppM (AppM)
import Bin.AppM as AppM
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Int as Int
import Data.Map as Map
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (alaF)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Lib.Foreign.Octokit as Octokit
import Lib.Nix.Manifest (Manifests, PursManifest, SpagoManifest)
import Lib.Nix.Manifest as NixManifest
import Lib.Utils as Utils
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process

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

  AppM.runAppM { octokit } $ case mode of
    Verify dir -> do
      Console.log "Verifying manifests..."
      manifests <- readManifests dir
      let pursEntries = alaF Additive foldMap Map.size (Map.values manifests.purs)
      Console.log $ "Successfully parsed purs.json with " <> Int.toStringAs Int.decimal pursEntries <> " entries."
      let spagoEntries = Map.size manifests.spago
      Console.log $ "Successfully parsed spago.json with " <> Int.toStringAs Int.decimal spagoEntries <> " entries."

    Prefetch dir -> do
      Console.log "Prefetching..."
      manifests <- readManifests dir
      updates <- fetchUpdates manifests

      if Map.size updates.purs > 0 then
        Console.log $ "New purs releases: " <> Utils.printJson NixManifest.pursManifestCodec updates.purs
      else Console.log "No new purs releases."

      if Map.size updates.spago > 0 then
        Console.log $ "New spago releases: " <> Utils.printJson NixManifest.spagoManifestCodec updates.spago
      else Console.log "No new spago releases."

    Update dir commit -> do
      case commit of
        DoCommit ->
          Console.log "Updating, committing results, and opening a pull request if necessary..."
        NoCommit ->
          Console.log "Updating locally only (not committing results)"

      manifests <- readManifests dir
      updates <- fetchUpdates manifests

      if Map.size updates.purs > 0 then
        Console.log $ "New purs releases: " <> Utils.printJson NixManifest.pursManifestCodec updates.purs
      else Console.log "No new purs releases."

      if Map.size updates.spago > 0 then
        Console.log $ "New spago releases: " <> Utils.printJson NixManifest.spagoManifestCodec updates.spago
      else Console.log "No new spago releases."
      pure unit

readManifests :: FilePath -> AppM Manifests
readManifests dir = do
  purs <- readPursManifest dir
  spago <- readSpagoManifest dir
  pure { purs, spago }

readPursManifest :: FilePath -> AppM PursManifest
readPursManifest dir = Utils.readJsonFile (Path.concat [ dir, "purs.json" ]) NixManifest.pursManifestCodec

readSpagoManifest :: FilePath -> AppM SpagoManifest
readSpagoManifest dir = Utils.readJsonFile (Path.concat [ dir, "spago.json" ]) NixManifest.spagoManifestCodec

-- | Retrieve all relevant releases for the supported tools which do not already
-- | exist in the input manifests.
fetchUpdates :: Manifests -> AppM Manifests
fetchUpdates existing = do
  releases <- fetchReleases
  pure
    { spago: Map.difference releases.spago existing.spago
    , purs: Map.difference releases.purs existing.purs
    }

-- | Retrieve all relevant releases for the various supported tools from GitHub.
fetchReleases :: AppM Manifests
fetchReleases = do
  purs <- fetchPursReleases
  spago <- fetchSpagoReleases
  pure { purs, spago }

-- FIXME: Unimplemented.
fetchPursReleases :: AppM PursManifest
fetchPursReleases = pure Map.empty

-- TODO: Spago's alpha does not currently have any official releases, so we
-- don't fetch anything.
fetchSpagoReleases :: AppM SpagoManifest
fetchSpagoReleases = pure Map.empty
