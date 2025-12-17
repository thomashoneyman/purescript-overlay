module Bin.AppM where

import Prelude

import Bin.CLI (Verbosity(..))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Reader as ReaderT
import Data.Either (Either)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Lib.Foreign.Octokit (GitHubError, Octokit)
import Lib.Git (GitM(..))
import Lib.GitHub (GitHubM(..))
import Lib.Nix.Manifest (ManifestCodec, NamedManifest)
import Lib.Nix.Manifest as Nix.Manifest
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
  , verbosity :: Verbosity
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

instance MonadApp AppM where
  runGitHubM (GitHubM run) = do
    { octokit, verbosity } <- ask
    let debugFn msg = when (verbosity >= Verbose) $ Console.log $ "[DEBUG] " <> msg
    liftAff $ runReaderT (runExceptT run) { octokit, debug: debugFn }

  runGitM (GitM run) = do
    { tmpDir, gitBranch, verbosity } <- ask
    let debugFn msg = when (verbosity >= Verbose) $ Console.log $ "[DEBUG] " <> msg
    liftAff $ runReaderT (runExceptT run) { cwd: tmpDir, branch: gitBranch, debug: debugFn }

runAppM :: forall a. Env -> AppM a -> Aff a
runAppM env (AppM run) = runReaderT run env

-- | Read the named manifest (tool channel -> version mappings)
readNamedManifest :: AppM NamedManifest
readNamedManifest = do
  { manifestDir } <- ask
  Utils.readJsonFile (Path.concat [ manifestDir, Nix.Manifest.namedPath ]) Nix.Manifest.namedManifestCodec

-- | Write the named manifest
writeNamedManifest :: NamedManifest -> AppM Unit
writeNamedManifest manifest = do
  { manifestDir } <- ask
  Utils.writeJsonFile (Path.concat [ manifestDir, Nix.Manifest.namedPath ]) Nix.Manifest.namedManifestCodec manifest

-- | Read a tool's manifest using its codec
readManifest :: forall a. ManifestCodec a -> AppM a
readManifest { codec, tool } = do
  { manifestDir } <- ask
  Utils.readJsonFile (Path.concat [ manifestDir, Nix.Manifest.filename tool ]) codec

-- | Write a tool's manifest using its codec
writeManifest :: forall a. ManifestCodec a -> a -> AppM Unit
writeManifest { codec, tool } manifest = do
  { manifestDir } <- ask
  Utils.writeJsonFile (Path.concat [ manifestDir, Nix.Manifest.filename tool ]) codec manifest

-- | Log a message (always shown unless Quiet)
log :: String -> AppM Unit
log msg = do
  { verbosity } <- ask
  when (verbosity > Quiet) $ Console.log $ "[INFO] " <> msg

-- | Log a message only in Verbose mode
debug :: String -> AppM Unit
debug msg = do
  { verbosity } <- ask
  when (verbosity >= Verbose) $ Console.log $ "[DEBUG] " <> msg
