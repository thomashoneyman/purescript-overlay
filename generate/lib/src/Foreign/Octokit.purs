-- | Low-level bindings to Octokit and its request functions.
module Lib.Foreign.Octokit
  ( Address
  , Base64Content(..)
  , GitHubAPIError
  , GitHubError(..)
  , GitHubRoute
  , GitHubToken(..)
  , JSArgs
  , Octokit
  , PullRequest
  , RateLimit
  , Release
  , ReleaseAsset
  , Request
  , decodeBase64Content
  , githubErrorCodec
  , newAuthOctokit
  , newOctokit
  , printGitHubError
  , releaseAssetCodec
  , releaseCodec
  , request
  , requestCreatePullRequest
  , requestGetCommitDate
  , requestGetContent
  , requestGetLatestRelease
  , requestGetPullRequests
  , requestGetRefCommitSha
  , requestGetReleaseByTagName
  , requestListReleases
  , requestRateLimit
  ) where

import Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array as Array
import Data.Codec as Codec
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJ.Record
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (class Newtype, unwrap)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Profunctor as Profunctor
import Data.String as String
import Data.String.Base64 as Base64
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Uncurried (EffectFn1, EffectFn6, runEffectFn1, runEffectFn6)
import Foreign.Object (Object)
import Foreign.Object as Object
import JSON (JSON, JObject)
import JSON as JSON
import JSON.Object as JSON.Object
import Node.Path (FilePath)
import Registry.Internal.Codec as Registry.Codec
import Unsafe.Coerce (unsafeCoerce)

newtype GitHubToken = GitHubToken String

derive instance Newtype GitHubToken _

-- | An instance of GitHub's Octokit client
foreign import data Octokit :: Type

foreign import newOctokitImpl :: EffectFn1 (Nullable GitHubToken) Octokit

newOctokit :: forall m. MonadEffect m => m Octokit
newOctokit = liftEffect $ runEffectFn1 newOctokitImpl Nullable.null

newAuthOctokit :: forall m. MonadEffect m => GitHubToken -> m Octokit
newAuthOctokit token = liftEffect $ runEffectFn1 newOctokitImpl (Nullable.notNull token)

-- | A newline-delimited base64-encoded file retrieved from the GitHub API
newtype Base64Content = Base64Content String

derive instance Newtype Base64Content _

decodeBase64Content :: Base64Content -> Either String String
decodeBase64Content (Base64Content string) =
  case traverse Base64.decode $ String.split (String.Pattern "\n") string of
    Left error -> Left $ Aff.message error
    Right values -> Right $ Array.fold values

-- | The address of a GitHub repository as a owner/repo pair.
type Address = { owner :: String, repo :: String }

-- | List repository releases
requestListReleases :: Address -> Request (Array Release)
requestListReleases address =
  { route: GitHubRoute GET [ "repos", address.owner, address.repo, "releases" ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginate: true
  , codec: CJ.array releaseCodec
  }

-- | Get the latest release for the given repository
requestGetLatestRelease :: Address -> Request Release
requestGetLatestRelease address =
  { route: GitHubRoute GET [ "repos", address.owner, address.repo, "releases", "latest" ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginate: false
  , codec: releaseCodec
  }

-- | Get a specific release associated with a tag name
requestGetReleaseByTagName :: Address -> String -> Request Release
requestGetReleaseByTagName address tag =
  { route: GitHubRoute GET [ "repos", address.owner, address.repo, "releases", "tags", tag ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginate: true
  , codec: releaseCodec
  }

-- | A GitHub release
type Release =
  { url :: String
  , tag :: String
  , draft :: Boolean
  , prerelease :: Boolean
  , assets :: Array ReleaseAsset
  , createdAt :: DateTime
  , publishedAt :: DateTime
  }

-- Internal representation matching GitHub API
type ReleaseRep =
  { html_url :: String
  , tag_name :: String
  , created_at :: DateTime
  , published_at :: DateTime
  , draft :: Boolean
  , prerelease :: Boolean
  , assets :: Array ReleaseAsset
  }

releaseCodec :: CJ.Codec Release
releaseCodec = Profunctor.dimap toRep fromRep $ CJ.Record.object
  { html_url: CJ.string
  , tag_name: CJ.string
  , created_at: Registry.Codec.iso8601DateTime
  , published_at: Registry.Codec.iso8601DateTime
  , draft: CJ.boolean
  , prerelease: CJ.boolean
  , assets: CJ.array releaseAssetCodec
  }
  where
  toRep :: Release -> ReleaseRep
  toRep { url, tag, draft, prerelease, assets, createdAt, publishedAt } =
    { html_url: url
    , tag_name: tag
    , created_at: createdAt
    , published_at: publishedAt
    , draft
    , prerelease
    , assets
    }

  fromRep :: ReleaseRep -> Release
  fromRep { html_url, tag_name, draft, prerelease, assets, created_at, published_at } =
    { url: html_url
    , tag: tag_name
    , createdAt: created_at
    , publishedAt: published_at
    , draft
    , prerelease
    , assets
    }

-- | A GitHub release asset
type ReleaseAsset =
  { name :: String
  , downloadUrl :: String
  }

type ReleaseAssetRep =
  { browser_download_url :: String
  , name :: String
  }

releaseAssetCodec :: CJ.Codec ReleaseAsset
releaseAssetCodec = Profunctor.dimap toRep fromRep $ CJ.Record.object
  { browser_download_url: CJ.string
  , name: CJ.string
  }
  where
  toRep :: ReleaseAsset -> ReleaseAssetRep
  toRep { downloadUrl, name } = { browser_download_url: downloadUrl, name }

  fromRep :: ReleaseAssetRep -> ReleaseAsset
  fromRep { browser_download_url, name } = { downloadUrl: browser_download_url, name }

-- | Fetch a specific file from the provided repository at the given ref and
-- | filepath. Filepaths should lead to a single file from the root of the repo.
requestGetContent :: { address :: Address, ref :: String, path :: FilePath } -> Request Base64Content
requestGetContent { address, ref, path } =
  { route: GitHubRoute GET [ "repos", address.owner, address.repo, "contents", path ] (Map.singleton "ref" ref)
  , headers: Object.empty
  , args: noArgs
  , paginate: false
  , codec: Profunctor.dimap toRep fromRep $ CJ.Record.object
      { data: CJ.Record.object
          { type: CJ.string
          , encoding: CJ.string
          , content: CJ.string
          }
      }
  }
  where
  toRep (Base64Content str) = { data: { type: "file", encoding: "base64", content: str } }
  fromRep { data: { content } } = Base64Content content

-- | Fetch the commit SHA for a given ref on a GitHub repository
requestGetRefCommitSha :: { address :: Address, ref :: String } -> Request String
requestGetRefCommitSha { address, ref } =
  { route: GitHubRoute GET [ "repos", address.owner, address.repo, "git", "ref", ref ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginate: false
  , codec: Profunctor.dimap (\sha -> { object: { sha } }) _.object.sha $ CJ.Record.object
      { object: CJ.Record.object { sha: CJ.string } }
  }

-- | Fetch the date associated with a given commit, in the RFC3339String format.
requestGetCommitDate :: { address :: Address, commitSha :: String } -> Request DateTime
requestGetCommitDate { address, commitSha } =
  { route: GitHubRoute GET [ "repos", address.owner, address.repo, "git", "commits", commitSha ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginate: false
  , codec: Profunctor.dimap (\date -> { committer: { date } }) _.committer.date $ CJ.Record.object
      { committer: CJ.Record.object { date: Registry.Codec.iso8601DateTime } }
  }

-- | The 'head' branch is the one where changes have been implemented. The
-- | 'base' branch is the branch to merge into.
type PullContent = { head :: String, base :: String, title :: String, body :: String }

-- | Create a new pull request
requestCreatePullRequest :: { address :: Address, content :: PullContent } -> Request { url :: String }
requestCreatePullRequest { address, content: { title, body, head, base } } =
  { route: GitHubRoute POST [ "repos", address.owner, address.repo, "pulls" ] Map.empty
  , headers: Object.empty
  , args: unsafeToJSArgs { title, body, head, base }
  , paginate: false
  , codec: CJ.Record.object { url: CJ.string }
  }

type PullRequest =
  { url :: String
  , state :: String
  , number :: Int
  , title :: String
  }

-- | Get pull requests
requestGetPullRequests :: Address -> Request (Array PullRequest)
requestGetPullRequests address =
  { route: GitHubRoute GET [ "repos", address.owner, address.repo, "pulls" ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginate: true
  , codec: CJ.array $ CJ.Record.object
      { url: CJ.string
      , state: CJ.string
      , number: CJ.int
      , title: CJ.string
      }
  }

type RateLimit =
  { limit :: Int
  , remaining :: Int
  , resetTime :: Maybe Instant
  }

type RateLimitRep =
  { data ::
      { resources ::
          { core ::
              { limit :: Int
              , remaining :: Int
              , reset :: Number
              }
          }
      }
  }

requestRateLimit :: Request RateLimit
requestRateLimit =
  { route: GitHubRoute GET [ "rate_limit" ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginate: false
  , codec: Profunctor.dimap toRep fromRep $ CJ.Record.object
      { data: CJ.Record.object
          { resources: CJ.Record.object
              { core: CJ.Record.object
                  { limit: CJ.int
                  , remaining: CJ.int
                  , reset: CJ.number
                  }
              }
          }
      }
  }
  where
  toRep :: RateLimit -> RateLimitRep
  toRep { limit, remaining, resetTime } =
    let
      reset = Maybe.fromMaybe (-9999.0) ((unwrap <<< Instant.unInstant) <$> resetTime)
    in
      { data: { resources: { core: { limit, remaining, reset } } } }

  fromRep :: RateLimitRep -> RateLimit
  fromRep { data: { resources: { core: { limit, remaining, reset } } } } =
    { limit, remaining, resetTime: Instant.instant $ Aff.Milliseconds $ reset * 1000.0 }

-- | A route for the GitHub API, ie. "GET /repos/purescript/registry/tags".
data GitHubRoute = GitHubRoute Method (Array String) (Map String String)

derive instance Eq GitHubRoute
derive instance Ord GitHubRoute

-- | Format a route as a usable GitHub route for Octokit
printGitHubRoute :: GitHubRoute -> String
printGitHubRoute (GitHubRoute method segments params) = show method <> " " <> printPath <> printParams
  where
  printPath = Array.foldMap (append "/") segments
  printParams = case Map.size params of
    0 -> ""
    _ -> append "?" $ String.joinWith "&" $ map (\(Tuple key val) -> key <> "=" <> val) $ Map.toUnfoldable params

-- | An opaque type for PureScript types we want to pass directly to JavaScript
data JSArgs

unsafeToJSArgs :: forall a. Record a -> JSArgs
unsafeToJSArgs = unsafeCoerce

noArgs :: JSArgs
noArgs = unsafeToJSArgs {}

type Request a =
  { route :: GitHubRoute
  , headers :: Object String
  , args :: JSArgs
  , paginate :: Boolean
  , codec :: CJ.Codec a
  }

foreign import requestImpl :: forall r. EffectFn6 Octokit String (Object String) JSArgs (Object JSON -> r) (JSON -> r) (Promise r)
foreign import paginateImpl :: forall r. EffectFn6 Octokit String (Object String) JSArgs (Object JSON -> r) (JSON -> r) (Promise r)

-- | Make a request to the GitHub API
request :: forall m a. MonadAff m => Octokit -> Request a -> m (Either GitHubError a)
request octokit { route, headers, args, paginate, codec } = do
  let printedRoute = printGitHubRoute route
  Console.log $ "Octokit: requesting " <> printedRoute
  result <- liftAff $ Promise.toAffE $ runEffectFn6 (if paginate then paginateImpl else requestImpl) octokit printedRoute headers args Left Right
  pure $ case result of
    Left githubError -> case decodeGitHubAPIError githubError of
      Left decodeError -> Left $ UnexpectedError decodeError
      Right decoded -> Left $ APIError decoded
    Right json -> case CJ.decode codec json of
      Left decodeError -> Left $ DecodeError { error: CJ.DecodeError.print decodeError, raw: JSON.printIndented json }
      Right parsed -> Right parsed
  where
  decodeGitHubAPIError :: Object JSON -> Either String GitHubAPIError
  decodeGitHubAPIError object = do
    let jObject = JSON.Object.fromFoldableWithIndex object
    statusCode <- atKey "status" CJ.int jObject
    message <- case statusCode of
      304 -> pure ""
      500 -> pure "Internal server error"
      _ -> atKey "response" CJ.jobject jObject >>= \respObj ->
        atKey "data" CJ.jobject respObj >>= \dataObj ->
          atKey "message" CJ.string dataObj
    pure { statusCode, message }

type GitHubAPIError =
  { statusCode :: Int
  , message :: String
  }

githubApiErrorCodec :: CJ.Codec GitHubAPIError
githubApiErrorCodec = CJ.Record.object
  { statusCode: CJ.int
  , message: CJ.string
  }

data GitHubError
  = UnexpectedError String
  | APIError GitHubAPIError
  | DecodeError { error :: String, raw :: String }

derive instance Eq GitHubError
derive instance Ord GitHubError

githubErrorCodec :: CJ.Codec GitHubError
githubErrorCodec = Profunctor.dimap toRep fromRep $ CJ.Record.object
  { tag: CJ.string
  , value: CJ.json
  }
  where
  toRep = case _ of
    UnexpectedError err -> { tag: "unexpectedError", value: Codec.encode CJ.string err }
    APIError err -> { tag: "apiError", value: Codec.encode githubApiErrorCodec err }
    DecodeError err -> { tag: "decodeError", value: Codec.encode (CJ.Record.object { error: CJ.string, raw: CJ.string }) err }

  fromRep { tag, value } = case tag of
    "unexpectedError" -> case CJ.decode CJ.string value of
      Right err -> UnexpectedError err
      Left _ -> UnexpectedError "Failed to decode error"
    "apiError" -> case CJ.decode githubApiErrorCodec value of
      Right err -> APIError err
      Left _ -> UnexpectedError "Failed to decode API error"
    "decodeError" -> case CJ.decode (CJ.Record.object { error: CJ.string, raw: CJ.string }) value of
      Right err -> DecodeError err
      Left _ -> UnexpectedError "Failed to decode decode error"
    _ -> UnexpectedError $ "Unknown error tag: " <> tag

printGitHubError :: GitHubError -> String
printGitHubError = case _ of
  UnexpectedError message -> Array.fold
    [ "Unexpected error: "
    , message
    ]
  APIError fields -> Array.fold
    [ "GitHub API error ("
    , Int.toStringAs Int.decimal fields.statusCode
    , "): "
    , fields.message
    ]
  DecodeError { error, raw } -> Array.fold
    [ "Decoding error: "
    , error
    , "\n\nRaw:\n"
    , raw
    ]

atKey :: forall a. String -> CJ.Codec a -> JObject -> Either String a
atKey key codec object =
  case JSON.Object.lookup key object of
    Nothing -> Left ("Missing key: " <> key)
    Just json -> case CJ.decode codec json of
      Left err -> Left (CJ.DecodeError.print err)
      Right val -> Right val
