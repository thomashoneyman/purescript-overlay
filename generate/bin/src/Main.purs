module Bin.Main where

import Prelude

import Bin.AppM as AppM
import Bin.CLI (Command(..), Commit(..))
import Bin.CLI as Bin.CLI
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
import Effect.Now as Now
import Lib.Foreign.Octokit as Octokit
import Lib.Foreign.Tmp as Tmp
import Lib.Git as Git
import Lib.GitHub as GitHub
import Lib.Nix.Manifest as Nix.Manifest
import Lib.SemVer as SemVer
import Lib.Utils (die)
import Lib.Utils as Utils
import Node.Path as Path

main :: Effect Unit
main = Aff.launchAff_ do
  mode <- Bin.CLI.run

  -- Set up the environment...
  tmp <- Tmp.mkTmpDir
  now <- liftEffect Now.now
  let random = Number.toString $ un Milliseconds $ Instant.unInstant now
  let branch = "generate/" <> fromMaybe random (String.stripSuffix (String.Pattern ".0") random)

  case mode of
    Verify { dir, verbosity } -> do
      let envFile = Path.concat [ dir, "..", "generate", ".env" ]
      liftAff $ Env.loadEnvFile envFile
      token <- Env.lookupOptional Env.githubToken
      octokit <- case token of
        Nothing -> Octokit.newOctokit
        Just tok -> Octokit.newAuthOctokit tok

      AppM.runAppM { octokit, manifestDir: dir, gitBranch: branch, tmpDir: tmp, verbosity } do
        Run.verifyPurs
        Run.verifySpago
        Run.verifyPursTidy
        Run.verifyPursBackendEs
        Run.verifyPursLanguageServer

    Prefetch { dir, verbosity } -> do
      let envFile = Path.concat [ dir, "..", "generate", ".env" ]
      liftAff $ Env.loadEnvFile envFile
      token <- Env.lookupOptional Env.githubToken
      octokit <- case token of
        Nothing -> Octokit.newOctokit
        Just tok -> Octokit.newAuthOctokit tok

      AppM.runAppM { octokit, manifestDir: dir, gitBranch: branch, tmpDir: tmp, verbosity } do
        Run.prefetchPurs >>= \updates ->
          when (not $ Map.isEmpty updates) do
            AppM.debug $ "New purs releases: " <> Utils.printJson Nix.Manifest.githubBinaryManifestCodec updates

        Run.prefetchSpago >>= \updates ->
          when (not $ Map.isEmpty updates) do
            AppM.debug $ "New spago releases: " <> Utils.printJson Nix.Manifest.combinedManifestCodec updates

        Run.prefetchPursTidy >>= \updates ->
          when (not $ Map.isEmpty updates) do
            AppM.debug $ "New purs-tidy releases: " <> Utils.printJson Nix.Manifest.npmRegistryManifestCodec updates

        Run.prefetchPursBackendEs >>= \updates ->
          when (not $ Map.isEmpty updates) do
            AppM.debug $ "New purs-backend-es releases: " <> Utils.printJson Nix.Manifest.npmRegistryManifestCodec updates

        Run.prefetchPursLanguageServer >>= \updates ->
          when (not $ Map.isEmpty updates) do
            AppM.debug $ "New purescript-language-server releases: " <> Utils.printJson Nix.Manifest.npmRegistryManifestCodec updates

    Update { dir, verbosity, commit } -> do
      let envFile = Path.concat [ dir, "..", "generate", ".env" ]
      liftAff $ Env.loadEnvFile envFile
      octokit <- case commit of
        DoCommit -> do
          token <- Env.lookupRequired Env.githubToken
          Octokit.newAuthOctokit token
        NoCommit -> do
          Env.lookupOptional Env.githubToken >>= case _ of
            Nothing -> Octokit.newOctokit
            Just tok -> Octokit.newAuthOctokit tok

      AppM.runAppM { octokit, manifestDir: dir, gitBranch: branch, tmpDir: tmp, verbosity } do
        pursUpdates <- Run.prefetchPurs
        spagoUpdates <- Run.prefetchSpago
        pursTidyUpdates <- Run.prefetchPursTidy
        pursBackendEsUpdates <- Run.prefetchPursBackendEs
        pursLanguageServerUpdates <- Run.prefetchPursLanguageServer

        case commit of
          NoCommit -> do
            AppM.debug "Updating locally only (not committing results)"
            when (not $ Map.isEmpty pursUpdates) do
              AppM.debug "Writing purs updates to disk"
              Run.writePursUpdates pursUpdates

            when (not $ Map.isEmpty spagoUpdates) do
              AppM.debug "Writing spago updates to disk"
              Run.writeSpagoUpdates spagoUpdates

            when (not $ Map.isEmpty pursTidyUpdates) do
              AppM.debug "Writing purs-tidy updates to disk"
              Run.writePursTidyUpdates pursTidyUpdates

            when (not $ Map.isEmpty pursBackendEsUpdates) do
              AppM.debug "Writing purs-backend-es updates to disk"
              Run.writePursBackendEsUpdates pursBackendEsUpdates

            when (not $ Map.isEmpty pursLanguageServerUpdates) do
              AppM.debug "Writing purescript-language-server updates to disk"
              Run.writePursLanguageServerUpdates pursLanguageServerUpdates

          DoCommit -> do
            AppM.debug "Cloning purescript-nix, opening a branch, committing, and opening a pull request"
            token <- Env.lookupRequired Env.githubToken

            -- We switch the manifest dir from the user-provided input to the
            -- cloned repo before we do anything else.
            Reader.local (\env -> env { manifestDir = Path.concat [ env.tmpDir, "purescript-nix", "manifests" ] }) do
              if Map.isEmpty pursUpdates && Map.isEmpty spagoUpdates && Map.isEmpty pursTidyUpdates && Map.isEmpty pursBackendEsUpdates && Map.isEmpty pursLanguageServerUpdates then
                AppM.log "No new releases."
              else do
                AppM.debug "Writing to disk and committing"
                void $ AppM.runGitM Git.gitCloneUpstream

                Run.writePursUpdates pursUpdates
                Run.writeSpagoUpdates spagoUpdates
                Run.writePursTidyUpdates pursTidyUpdates
                Run.writePursBackendEsUpdates pursBackendEsUpdates
                Run.writePursLanguageServerUpdates pursLanguageServerUpdates

                -- TODO: Commit message could be a lot more informative.
                commitResult <- AppM.runGitM do
                  Git.gitCommitManifests "Update manifests" >>= case _ of
                    Git.NothingToCommit -> do
                      die "No files were changed, not committing."
                    Git.Committed ->
                      Git.debugLog "Committed changes"

                case commitResult of
                  Left error -> die error
                  Right _ -> pure unit

                let
                  pursVersions = Map.keys pursUpdates
                  spagoVersions = Map.keys spagoUpdates
                  pursTidyVersions = Map.keys pursTidyUpdates
                  pursBackendEsVersions = Map.keys pursBackendEsUpdates
                  pursLanguageServerVersions = Map.keys pursLanguageServerUpdates

                  commitMsg = "Update " <> String.trim
                    ( String.joinWith " "
                        [ guard (Set.size pursVersions > 0) $ "purs (" <> String.joinWith ", " (Set.toUnfoldable (Set.map SemVer.print pursVersions)) <> ")"
                        , guard (Set.size spagoVersions > 0) $ "spago (" <> String.joinWith ", " (Set.toUnfoldable (Set.map SemVer.print spagoVersions)) <> ")"
                        , guard (Set.size pursTidyVersions > 0) $ "purs-tidy (" <> String.joinWith ", " (Set.toUnfoldable (Set.map SemVer.print pursTidyVersions)) <> ")"
                        , guard (Set.size pursBackendEsVersions > 0) $ "purs-backend-es (" <> String.joinWith ", " (Set.toUnfoldable (Set.map SemVer.print pursBackendEsVersions)) <> ")"
                        , guard (Set.size pursLanguageServerVersions > 0) $ "purescript-language-server (" <> String.joinWith ", " (Set.toUnfoldable (Set.map SemVer.print pursLanguageServerVersions)) <> ")"
                        ]
                    )

                existing <- AppM.runGitHubM GitHub.getPullRequests >>= case _ of
                  Left error -> do
                    die $ Octokit.printGitHubError error
                  Right existing -> pure existing

                -- TODO: Title comparison is a bit simplistic. Better to compare
                -- on like the new hashes or something?
                createPullResult <- case Array.find (eq commitMsg <<< _.title) existing of
                  Nothing -> do
                    pushResult <- AppM.runGitM $ Git.gitPushBranch token >>= case _ of
                      Git.NothingToPush -> do
                        die "Did not push branch because we're up-to-date (expected to push change)."
                      Git.Pushed ->
                        Git.debugLog "Pushed changes"
                    case pushResult of
                      Left error -> do
                        die error
                      Right _ ->
                        AppM.runGitHubM $ GitHub.createPullRequest { title: commitMsg, body: commitMsg, branch }

                  Just pull -> do
                    die $ "A pull request with this title is already open:\n" <> pull.url

                case createPullResult of
                  Left error -> do
                    die $ "Failed to create pull request:\n" <> Octokit.printGitHubError error
                  Right { url } -> do
                    AppM.log $ "Created pull request: " <> url
