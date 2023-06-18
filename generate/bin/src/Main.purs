module Bin.Main where

import Prelude

import Bin.AppM as AppM
import Bin.CLI (Command(..), Commit(..))
import Bin.CLI as CLI
import Bin.Env as Env
import Bin.Run as Run
import Control.Monad.Reader as Reader
import Data.Array as Array
import Data.DateTime.Instant as Instant
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.Newtype (un)
import Data.Number.Format as Number
import Data.Set as Set
import Data.String as String
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now as Now
import Lib.Foreign.Octokit as Octokit
import Lib.Foreign.Tmp as Tmp
import Lib.Git as Git
import Lib.GitHub as GitHub
import Lib.Nix.Manifest as Nix.Manifest
import Lib.SemVer as SemVer
import Lib.Utils as Utils
import Node.Path as Path
import Node.Process as Process

main :: Effect Unit
main = Aff.launchAff_ do
  mode <- CLI.run

  -- Set up the environment...
  tmp <- Tmp.mkTmpDir
  now <- liftEffect Now.now
  let random = Number.toString $ un Milliseconds $ Instant.unInstant now
  let branch = "generate/" <> fromMaybe random (String.stripSuffix (String.Pattern ".0") random)

  case mode of
    Verify dir -> do
      let envFile = Path.concat [ dir, "..", "generate", ".env" ]
      Console.log $ "Loading .env file from " <> envFile
      liftAff $ Env.loadEnvFile envFile
      token <- Env.lookupOptional Env.githubToken
      octokit <- case token of
        Nothing -> Octokit.newOctokit
        Just tok -> Octokit.newAuthOctokit tok

      AppM.runAppM { octokit, manifestDir: dir, gitBranch: branch, tmpDir: tmp } do
        Console.log "Verifying manifests..."
        Run.verifyPurs
        Run.verifySpago

    Prefetch dir -> do
      let envFile = Path.concat [ dir, "..", "generate", ".env" ]
      Console.log $ "Loading .env file from " <> envFile
      liftAff $ Env.loadEnvFile envFile
      token <- Env.lookupOptional Env.githubToken
      octokit <- case token of
        Nothing -> Octokit.newOctokit
        Just tok -> Octokit.newAuthOctokit tok

      AppM.runAppM { octokit, manifestDir: dir, gitBranch: branch, tmpDir: tmp } do
        Console.log "Prefetching new releases..."

        Run.prefetchPurs >>= \updates ->
          if Map.isEmpty updates then
            Console.log "No new purs releases."
          else
            Console.log $ "New purs releases: " <> Utils.printJson Nix.Manifest.pursManifestCodec updates

        Run.prefetchSpago >>= \updates ->
          if Map.isEmpty updates then
            Console.log "No new spago releases."
          else
            Console.log $ "New spago releases: " <> Utils.printJson Nix.Manifest.spagoManifestCodec updates

    -- TODO: Also update the named manifest file.
    Update dir commit -> do
      let envFile = Path.concat [ dir, "..", "generate", ".env" ]
      Console.log $ "Loading .env file from " <> envFile
      liftAff $ Env.loadEnvFile envFile
      octokit <- case commit of
        DoCommit -> do
          token <- Env.lookupRequired Env.githubToken
          Octokit.newAuthOctokit token
        NoCommit -> do
          Env.lookupOptional Env.githubToken >>= case _ of
            Nothing -> Octokit.newOctokit
            Just tok -> Octokit.newAuthOctokit tok

      AppM.runAppM { octokit, manifestDir: dir, gitBranch: branch, tmpDir: tmp } do
        pursUpdates <- Run.prefetchPurs
        spagoUpdates <- Run.prefetchSpago

        case commit of
          NoCommit -> do
            Console.log "Updating locally only (not committing results)"
            if Map.isEmpty pursUpdates then
              Console.log "No new purs releases."
            else do
              Console.log $ "New purs releases, writing to disk..."
              Run.writePursUpdates pursUpdates

            if Map.isEmpty spagoUpdates then
              Console.log "No new spago releases."
            else do
              Console.log $ "New spago releases, writing to disk..."
              Run.writeSpagoUpdates spagoUpdates

          DoCommit -> do
            Console.log "Cloning purescript-nix, opening a branch, committing, and opening a pull request..."
            token <- Env.lookupRequired Env.githubToken

            -- We switch the manifest dir from the user-provided input to the
            -- cloned repo before we do anything else.
            Reader.local (\env -> env { manifestDir = Path.concat [ env.tmpDir, "purescript-nix", "manifests" ] }) do
              if Map.isEmpty pursUpdates && Map.isEmpty spagoUpdates then
                Console.log "No new releases."
              else do
                Console.log "New purs releases, writing to disk..."
                void $ AppM.runGitM Git.gitCloneUpstream

                Run.writePursUpdates pursUpdates
                Run.writeSpagoUpdates spagoUpdates

                -- TODO: Commit message could be a lot more informative.
                commitResult <- AppM.runGitM do
                  Git.gitCommitManifests "Update manifests" >>= case _ of
                    Git.NothingToCommit -> do
                      Console.log "No files were changed, not committing."
                      liftEffect (Process.exit 1)
                    Git.Committed ->
                      Console.log "Committed changes!"

                case commitResult of
                  Left error -> Console.log error *> liftEffect (Process.exit 1)
                  Right _ -> pure unit

                let
                  pursVersions = Map.keys pursUpdates
                  spagoVersions = Map.keys spagoUpdates
                  title = "Update " <> String.trim
                    ( String.joinWith " "
                        [ guard (Set.size pursVersions > 0) $ "purs (" <> String.joinWith ", " (Set.toUnfoldable (Set.map SemVer.print pursVersions)) <> ")"
                        , guard (Set.size spagoVersions > 0) $ "spago (" <> String.joinWith ", " (Set.toUnfoldable (Set.map SemVer.print spagoVersions)) <> ")"
                        ]
                    )

                  -- TODO: What would be the most informative thing to do here?
                  body = "Update manifest files to new tooling versions."

                existing <- AppM.runGitHubM GitHub.getPullRequests >>= case _ of
                  Left error -> do
                    Console.log $ Octokit.printGitHubError error
                    liftEffect (Process.exit 1)
                  Right existing -> pure existing

                -- TODO: Title comparison is a bit simplistic. Better to compare
                -- on like the new hashes or something?
                createPullResult <- case Array.find (eq title <<< _.title) existing of
                  Nothing -> do
                    pushResult <- AppM.runGitM $ Git.gitPushBranch token >>= case _ of
                      Git.NothingToPush -> do
                        Console.log "Did not push branch because we're up-to-date (expected to push change)."
                        liftEffect (Process.exit 1)
                      Git.Pushed ->
                        Console.log "Pushed changes!"
                    case pushResult of
                      Left error -> do
                        Console.log error
                        liftEffect (Process.exit 1)
                      Right _ ->
                        AppM.runGitHubM $ GitHub.createPullRequest { title, body, branch }

                  Just pull -> do
                    Console.log "A pull request with this title is already open: "
                    Console.log pull.url
                    liftEffect (Process.exit 1)

                case createPullResult of
                  Left error -> do
                    Console.log "Failed to create pull request:"
                    Console.log $ Octokit.printGitHubError error
                    liftEffect (Process.exit 1)
                  Right { url } -> do
                    Console.log "Successfully created pull request!"
                    Console.log url
