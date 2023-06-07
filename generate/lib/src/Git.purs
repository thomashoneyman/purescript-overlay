module Lib.Git where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except as Except
import Control.Monad.Reader (class MonadAsk, ReaderT, ask)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Lib.Foreign.Octokit (GitHubToken(..))
import Node.Library.Execa as Execa
import Node.Path (FilePath)
import Node.Path as Path
import Parsing as Parsing
import Parsing.Combinators as Parsing.Combinators
import Parsing.String as Parsing.String
import Parsing.String.Basic as Parsing.String.Basic

type GitEnv = { cwd :: FilePath, branch :: String }

-- | A monad for executing Git CLI commands
newtype GitM a = GitM (ExceptT String (ReaderT GitEnv Aff) a)

derive instance Newtype (GitM a) _
derive newtype instance Functor GitM
derive newtype instance Apply GitM
derive newtype instance Applicative GitM
derive newtype instance Bind GitM
derive newtype instance Monad GitM
derive newtype instance MonadEffect GitM
derive newtype instance MonadAff GitM
derive newtype instance MonadAsk GitEnv GitM
derive newtype instance MonadThrow String GitM

-- | A Git tag
newtype Tag = Tag String

derive instance Newtype Tag _
derive newtype instance Eq Tag
derive newtype instance Ord Tag

-- | A Git commit hash
newtype CommitSha = CommitSha String

derive instance Newtype CommitSha _
derive newtype instance Eq CommitSha
derive newtype instance Ord CommitSha

type Committer = { name :: String, email :: String }

manifestBot :: Committer
manifestBot = { name: "robo-trh", email: "thomashoneyman@pm.me" }

repoName :: String
repoName = "purescript-nix"

-- | Clone the upstream repository and check out the indicated branch
gitCloneUpstream :: GitM Unit
gitCloneUpstream = do
  { cwd, branch } <- ask

  let
    url = "https://github.com/thomashoneyman/purescript-nix.git"
    inRepoErr error = " in local checkout " <> cwd <> ": " <> error

  git (Just cwd) [ "clone", url, repoName ] >>= case _ of
    Left err -> Except.throwError $ "Failed to clone repo at url " <> url <> inRepoErr err
    Right _ -> pure unit

  git (Just (Path.concat [ cwd, repoName ])) [ "checkout", "-b", branch ] >>= case _ of
    Left err -> Except.throwError $ "Failed to checkout branch " <> branch <> inRepoErr err
    Right _ -> pure unit

data CommitResult = Committed | NothingToCommit

derive instance Eq CommitResult

-- | Commit the manifests directory
gitCommitManifests :: String -> GitM CommitResult
gitCommitManifests message = do
  { cwd, branch } <- ask

  let
    inRepoErr error = " in local checkout " <> cwd <> ": " <> error
    repoPath = Path.concat [ cwd, repoName ]
    exec args onError = git (Just repoPath) args >>= case _ of
      Left error -> Except.throwError (onError error)
      Right output -> pure output

  -- First we fetch the origin to make sure we're up-to-date.
  status <- gitStatus
  when (status.branch /= branch) do
    Except.throwError $ "Expected to be on branch " <> branch <> " but branch " <> status.branch <> " is checked out in " <> cwd

  -- Then we ensure the commit metadata will match the expected committer
  _ <- exec [ "config", "user.name", manifestBot.name ] \error ->
    "Failed to configure git user name as " <> manifestBot.name <> inRepoErr error
  _ <- exec [ "config", "user.email", "<" <> manifestBot.email <> ">" ] \error ->
    "Failed to configure git user email as " <> manifestBot.email <> inRepoErr error

  -- Then we attempt to stage changes associated with the given commit paths
  let commitPaths = [ "manifests" ]
  _ <- exec (Array.cons "add" commitPaths) \error ->
    "Failed to add path(s) " <> String.joinWith ", " commitPaths <> inRepoErr error

  -- Git will error if we try to commit without any changes actually staged,
  -- so the below command lists file paths (--name-only) that have changed
  -- between the index and current HEAD (--cached), only including files that
  -- have been added or modified (--diff-filter=AM).
  staged <- exec [ "diff", "--name-only", "--cached", "--diff-filter=AM" ] \error ->
    "Failed to check whether any changes are staged " <> inRepoErr error

  -- If there are no staged files, then we have nothing to commit.
  if String.null staged then do
    Console.log $ "Not committing paths " <> String.joinWith ", " commitPaths <> " in " <> cwd <> " because no matching files have been modified."
    pure NothingToCommit
  -- But if there are staged files then we can commit them and report that
  -- we indeed had changes.
  else do
    _ <- exec [ "commit", "-m", message ] \error ->
      "Failed to commit path(s) " <> String.joinWith ", " commitPaths <> inRepoErr error
    pure Committed

data PushResult = Pushed | NothingToPush

derive instance Eq PushResult

-- | Push changes to the purescript-nix repository
gitPushBranch :: GitHubToken -> GitM PushResult
gitPushBranch token = do
  { cwd, branch } <- ask

  -- First we fetch the origin to make sure we're up-to-date.
  status <- gitStatus
  when (status.branch /= branch) do
    Console.log $ "Expected to be on branch " <> branch <> " in local checkout " <> cwd <> " but found branch " <> status.branch
    Except.throwError "Aborting 'git push'."

  let
    upstream = { owner: "thomashoneyman", repo: "purescript-nix" }
    inRepoErr error = " in local checkout " <> cwd <> ": " <> error
    repoPath = Path.concat [ cwd, repoName ]

    exec args onError = git (Just repoPath) args >>= case _ of
      Left error -> Except.throwError (onError error)
      Right output -> pure output

    origin :: String
    origin = Array.fold
      [ "https://"
      , "manifest-bot"
      , ":"
      , un GitHubToken token
      , "@github.com/"
      , upstream.owner
      , "/"
      , upstream.repo
      , ".git"
      ]

  _ <- exec [ "push", "--set-upstream", origin, branch ] \error ->
    "Failed to push to " <> origin <> " from " <> branch <> inRepoErr error

  pure Pushed

gitStatus :: GitM { branch :: String, dirty :: Maybe (NonEmptyArray String) }
gitStatus = do
  { cwd } <- ask

  let
    inRepoErr error = " in local checkout " <> cwd <> ": " <> error
    repoPath = Path.concat [ cwd, repoName ]
    exec args onError = git (Just repoPath) args >>= case _ of
      Left error -> Except.throwError (onError error)
      Right output -> pure output

  -- First we fetch the origin to make sure we're up-to-date.
  _ <- exec [ "fetch", "origin" ] \error ->
    "Failed to fetch origin " <> inRepoErr error

  -- Then we check the local status, which will return as its first line of
  -- output a status string like the below:
  --
  -- ## master...origin/master [ahead 3]
  statusOutput <- exec [ "status", "--short", "--branch", "--porcelain" ] \error ->
    "Failed to check status " <> inRepoErr error

  case Array.uncons (String.split (String.Pattern "\n") statusOutput) of
    Nothing -> Except.throwError "Output of git status did not contain any text."
    Just { head, tail } -> case Parsing.runParser head gitStatusParser of
      Left error -> Except.throwError $ "Could not parse git status " <> head <> inRepoErr (Parsing.parseErrorMessage error)
      Right { branch } -> pure { branch, dirty: NonEmptyArray.fromArray tail }

-- | The 'git status' command in short mode, with branches displayed, returns
-- | one of the following four formats as the first line of output:
-- |
-- | ## master
-- | ## master...origin/master
-- | ## master...origin/master [ahead 3]
-- | ## master...origin/master [behind 2]
-- |
-- | ...where the first means we have no upstream, the second means we are
-- | current, and the other two mean we are ahead or behind.
gitStatusParser :: Parsing.Parser String { branch :: String }
gitStatusParser = do
  _ <- Parsing.String.string "##"
  _ <- Parsing.String.Basic.whiteSpace
  branchChars <- Parsing.Combinators.manyTill Parsing.String.anyChar (Parsing.String.string "..." $> unit <|> Parsing.String.eof)
  let branch = CodeUnits.fromCharArray (Array.fromFoldable branchChars)
  pure { branch }

git :: forall m. MonadAff m => Maybe String -> Array String -> m (Either String String)
git cwd args = liftAff do
  spawned <- Execa.execa "git" args (_ { cwd = cwd })
  result <- spawned.result
  pure $ case result of
    Left error -> Left error.message
    Right { stdout } -> Right (String.trim stdout)
