module Bin.AppM where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Reader as ReaderT
import Data.Either (Either)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception as Exception
import Lib.Foreign.Octokit (GitHubError, Octokit)
import Lib.Git (GitM(..))
import Lib.GitHub (GitHubM(..))
import Lib.Nix.Manifest (CombinedManifest, GitHubBinaryManifest, NamedManifest, NPMRegistryManifest)
import Lib.Nix.Manifest as Nix.Manifest
import Lib.Nix.Manifest as Tool
import Lib.Tool (Tool(..))
import Lib.Utils as Utils
import Node.Path (FilePath)
import Node.Path as Path

-- | An app-specific class for functions we want to be made more convenient
class MonadApp m where
  runGitHubM :: forall a. GitHubM a -> m (Either GitHubError a)
  runGitM :: forall a. GitM a -> m (Either String a)

type Env =
  { octokit :: Octokit
  , tmpDir :: FilePath
  , gitBranch :: String
  , manifestDir :: FilePath
  }

newtype AppM a = AppM (ReaderT Env Aff a)

derive instance Newtype (AppM a) _
derive newtype instance Functor AppM
derive newtype instance Apply AppM
derive newtype instance Applicative AppM
derive newtype instance Bind AppM
derive newtype instance Monad AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAff AppM
derive newtype instance MonadAsk Env AppM
derive newtype instance MonadReader Env AppM

instance MonadThrow String AppM where
  throwError = liftEffect <<< Exception.throwException <<< Exception.error

instance MonadApp AppM where
  runGitHubM (GitHubM run) = do
    { octokit } <- ask
    liftAff $ runReaderT (runExceptT run) octokit

  runGitM (GitM run) = do
    { tmpDir, gitBranch } <- ask
    liftAff $ runReaderT (runExceptT run) { cwd: tmpDir, branch: gitBranch }

runAppM :: forall a. Env -> AppM a -> Aff a
runAppM env (AppM run) = runReaderT run env

getNamedManifestPath :: AppM FilePath
getNamedManifestPath = do
  { manifestDir } <- ask
  pure $ Path.concat [ manifestDir, Nix.Manifest.namedPath ]

readNamedManifest :: AppM NamedManifest
readNamedManifest = do
  path <- getNamedManifestPath
  Utils.readJsonFile path Nix.Manifest.namedManifestCodec

writeNamedManifest :: NamedManifest -> AppM Unit
writeNamedManifest manifest = do
  path <- getNamedManifestPath
  Utils.writeJsonFile path Nix.Manifest.namedManifestCodec manifest

getToolManifestPath :: Tool -> AppM FilePath
getToolManifestPath tool = do
  { manifestDir } <- ask
  pure $ Path.concat [ manifestDir, Tool.filename tool ]

readPursManifest :: AppM GitHubBinaryManifest
readPursManifest = do
  path <- getToolManifestPath Purs
  Utils.readJsonFile path Nix.Manifest.githubBinaryManifestCodec

writePursManifest :: GitHubBinaryManifest -> AppM Unit
writePursManifest manifest = do
  path <- getToolManifestPath Purs
  Utils.writeJsonFile path Nix.Manifest.githubBinaryManifestCodec manifest

readSpagoManifest :: AppM CombinedManifest
readSpagoManifest = do
  path <- getToolManifestPath Spago
  Utils.readJsonFile path Nix.Manifest.combinedManifestCodec

writeSpagoManifest :: CombinedManifest -> AppM Unit
writeSpagoManifest manifest = do
  path <- getToolManifestPath Spago
  Utils.writeJsonFile path Nix.Manifest.combinedManifestCodec manifest

readPursTidyManifest :: AppM NPMRegistryManifest
readPursTidyManifest = do
  path <- getToolManifestPath PursTidy
  Utils.readJsonFile path Nix.Manifest.npmRegistryManifestCodec

writePursTidyManifest :: NPMRegistryManifest -> AppM Unit
writePursTidyManifest manifest = do
  path <- getToolManifestPath PursTidy
  Utils.writeJsonFile path Nix.Manifest.npmRegistryManifestCodec manifest

readPursBackendEsManifest :: AppM NPMRegistryManifest
readPursBackendEsManifest = do
  path <- getToolManifestPath PursBackendEs
  Utils.readJsonFile path Nix.Manifest.npmRegistryManifestCodec

writePursBackendEsManifest :: NPMRegistryManifest -> AppM Unit
writePursBackendEsManifest manifest = do
  path <- getToolManifestPath PursBackendEs
  Utils.writeJsonFile path Nix.Manifest.npmRegistryManifestCodec manifest

readPursLanguageServerManifest :: AppM NPMRegistryManifest
readPursLanguageServerManifest = do
  path <- getToolManifestPath PursLanguageServer
  Utils.readJsonFile path Nix.Manifest.npmRegistryManifestCodec

writePursLanguageServerManifest :: NPMRegistryManifest -> AppM Unit
writePursLanguageServerManifest manifest = do
  path <- getToolManifestPath PursLanguageServer
  Utils.writeJsonFile path Nix.Manifest.npmRegistryManifestCodec manifest
