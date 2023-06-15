module Lib.GitHub where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Reader (class MonadAsk, ReaderT, ask)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Lib.Foreign.Octokit (GitHubError(..), Octokit, Release, Request, PullRequest)
import Lib.Foreign.Octokit as Octokit
import Lib.Git (CommitSha(..), Tag(..))
import Lib.Tool (Tool(..))
import Lib.Utils as Utils

-- | A monad for executing requests to GitHub
newtype GitHubM a = GitHubM (ExceptT GitHubError (ReaderT Octokit Aff) a)

derive instance Newtype (GitHubM a) _
derive newtype instance Functor GitHubM
derive newtype instance Apply GitHubM
derive newtype instance Applicative GitHubM
derive newtype instance Bind GitHubM
derive newtype instance Monad GitHubM
derive newtype instance MonadEffect GitHubM
derive newtype instance MonadAff GitHubM
derive newtype instance MonadAsk Octokit GitHubM
derive newtype instance MonadThrow GitHubError GitHubM

newtype GitHubToken = GitHubToken String

toolRepo :: Tool -> Octokit.Address
toolRepo = case _ of
  Purs -> { owner: "purescript", repo: "purescript" }
  Spago -> { owner: "purescript", repo: "spago" }
  PursTidy -> { owner: "natefaubion", repo: "purescript-tidy" }
  PursBackendEs -> { owner: "aristanetworks", repo: "purescript-backend-optimizer" }

listReleases :: Tool -> GitHubM (Array Release)
listReleases tool = do
  octokit <- ask
  let address = toolRepo tool
  Console.log $ "Listing releases for repo " <> address.owner <> "/" <> address.repo
  let req = requestWithBackoff octokit (Octokit.requestListReleases address)
  GitHubM $ ExceptT req

getLatestRelease :: Tool -> GitHubM Release
getLatestRelease tool = do
  octokit <- ask
  let address = toolRepo tool
  Console.log $ "Getting latest release for repo " <> address.owner <> "/" <> address.repo
  let req = requestWithBackoff octokit (Octokit.requestGetLatestRelease address)
  GitHubM $ ExceptT req

getReleaseByTagName :: Tool -> Tag -> GitHubM Release
getReleaseByTagName tool tag = do
  octokit <- ask
  let address = toolRepo tool
  Console.log $ "Getting release identified by tag " <> un Tag tag <> " in repo " <> address.owner <> "/" <> address.repo
  let req = requestWithBackoff octokit (Octokit.requestGetReleaseByTagName address (un Tag tag))
  GitHubM $ ExceptT req

getTagCommitSha :: Tool -> Tag -> GitHubM CommitSha
getTagCommitSha tool tag = do
  octokit <- ask
  let address = toolRepo tool
  Console.log $ "Getting commit SHA for tag " <> un Tag tag <> " in repo " <> address.owner <> "/" <> address.repo
  let req = requestWithBackoff octokit (Octokit.requestGetRefCommitSha { address, ref: un Tag tag })
  GitHubM $ ExceptT $ map CommitSha <$> req

getCommitDate :: Tool -> CommitSha -> GitHubM DateTime
getCommitDate tool sha = do
  octokit <- ask
  let address = toolRepo tool
  Console.log $ "Getting commit date for commit SHA " <> un CommitSha sha <> " in repo " <> address.owner <> "/" <> address.repo
  let req = requestWithBackoff octokit (Octokit.requestGetCommitDate { address, commitSha: un CommitSha sha })
  GitHubM $ ExceptT req

type PullRequestData =
  { title :: String
  , body :: String
  , branch :: String
  }

createPullRequest :: PullRequestData -> GitHubM { url :: String }
createPullRequest { title, body, branch } = do
  octokit <- ask
  let address = { owner: "thomashoneyman", repo: "purescript-nix" }
  let base = "main"
  Console.log $ "\nCreating pull request in repo " <> address.owner <> "/" <> address.repo <> " from branch " <> branch
  Console.log $ "Title:"
  Console.log title
  Console.log "Body"
  Console.log body
  let pull = { head: branch, base, title, body }
  let req = requestWithBackoff octokit (Octokit.requestCreatePullRequest { address, content: pull })
  GitHubM $ ExceptT req

getPullRequests :: GitHubM (Array PullRequest)
getPullRequests = do
  octokit <- ask
  let address = { owner: "thomashoneyman", repo: "purescript-nix" }
  Console.log $ "Fetching pull requests from  repo " <> address.owner <> "/" <> address.repo
  let req = requestWithBackoff octokit (Octokit.requestGetPullRequests address)
  GitHubM $ ExceptT req

-- | Apply exponential backoff to requests that hang, but without cancelling
-- | requests if we have reached our rate limit and have been throttled.
requestWithBackoff :: forall m a. MonadAff m => Octokit -> Request a -> m (Either GitHubError a)
requestWithBackoff octokit githubRequest = liftAff do
  let action = Octokit.request octokit githubRequest
  result <- Utils.withBackoff
    { delay: Aff.Milliseconds 5_000.0
    , action
    , shouldCancel: \_ -> Octokit.request octokit Octokit.requestRateLimit >>= case _ of
        Right { remaining } | remaining == 0 -> pure false
        _ -> pure true
    , shouldRetry: \attempt -> if attempt <= 3 then pure (Just action) else pure Nothing
    }
  case result of
    Nothing -> pure $ Left $ APIError { statusCode: 400, message: "Unable to reach GitHub servers." }
    Just accepted -> pure accepted
