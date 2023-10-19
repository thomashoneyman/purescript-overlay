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

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Argonaut
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonDecodeError, JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CA.Record
import Data.Codec.Argonaut.Variant as CA.Variant
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Newtype (class Newtype, unwrap)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Profunctor as Profunctor
import Data.String as String
import Data.String.Base64 as Base64
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Variant as Variant
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Uncurried (EffectFn1, EffectFn6, runEffectFn1, runEffectFn6)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Path (FilePath)
import Registry.Internal.Codec as Internal.Codec
import Registry.Internal.Codec as Registry.Codec
import Type.Proxy (Proxy(..))
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
  , codec: CA.array releaseCodec
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

releaseCodec :: JsonCodec Release
releaseCodec = Profunctor.dimap toJsonRep fromJsonRep $ CA.Record.object "Release"
  { html_url: CA.string
  , tag_name: CA.string
  , created_at: Registry.Codec.iso8601DateTime
  , published_at: Registry.Codec.iso8601DateTime
  , draft: CA.boolean
  , prerelease: CA.boolean
  , assets: CA.array releaseAssetCodec
  }
  where
  toJsonRep { url, tag, draft, prerelease, assets, createdAt, publishedAt } =
    { html_url: url
    , tag_name: tag
    , created_at: createdAt
    , published_at: publishedAt
    , draft
    , prerelease
    , assets
    }

  fromJsonRep { html_url, tag_name, draft, prerelease, assets, created_at, published_at } =
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

releaseAssetCodec :: JsonCodec ReleaseAsset
releaseAssetCodec = Profunctor.dimap toJsonRep fromJsonRep $ CA.Record.object "ReleaseAsset"
  { browser_download_url: CA.string
  , name: CA.string
  }
  where
  toJsonRep { downloadUrl, name } =
    { browser_download_url: downloadUrl
    , name
    }

  fromJsonRep { browser_download_url, name } =
    { downloadUrl: browser_download_url
    , name
    }

-- | Fetch a specific file  from the provided repository at the given ref and
-- | filepath. Filepaths should lead to a single file from the root of the repo.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/repos/getContent.md
requestGetContent :: { address :: Address, ref :: String, path :: FilePath } -> Request Base64Content
requestGetContent { address, ref, path } =
  { route: GitHubRoute GET [ "repos", address.owner, address.repo, "contents", path ] (Map.singleton "ref" ref)
  , headers: Object.empty
  , args: noArgs
  , paginate: false
  , codec: Profunctor.dimap toJsonRep fromJsonRep $ CA.Record.object "Content"
      { data: CA.Record.object "Content.data"
          { type: value "file"
          , encoding: value "base64"
          , content: CA.string
          }
      }
  }
  where
  value :: String -> JsonCodec String
  value expected = CA.codec'
    (\json -> CA.decode CA.string json >>= \decoded -> if decoded == expected then pure expected else Left (CA.UnexpectedValue json))
    (\_ -> CA.encode CA.string expected)

  toJsonRep (Base64Content str) = { data: { type: "file", encoding: "base64", content: str } }
  fromJsonRep { data: { content } } = Base64Content content

-- | Fetch the commit SHA for a given ref on a GitHub repository
requestGetRefCommitSha :: { address :: Address, ref :: String } -> Request String
requestGetRefCommitSha { address, ref } =
  { route: GitHubRoute GET [ "repos", address.owner, address.repo, "git", "ref", ref ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginate: false
  , codec: Profunctor.dimap toJsonRep fromJsonRep $ CA.Record.object "Ref"
      { object: CA.Record.object "Ref.object"
          { sha: CA.string }
      }
  }
  where
  toJsonRep sha = { object: { sha } }
  fromJsonRep = _.object.sha

-- | Fetch the date associated with a given commit, in the RFC3339String format.
requestGetCommitDate :: { address :: Address, commitSha :: String } -> Request DateTime
requestGetCommitDate { address, commitSha } =
  { route: GitHubRoute GET [ "repos", address.owner, address.repo, "git", "commits", commitSha ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginate: false
  , codec: Profunctor.dimap toJsonRep fromJsonRep $ CA.Record.object "Commit"
      { committer: CA.Record.object "Commit.committer" { date: Internal.Codec.iso8601DateTime } }
  }
  where
  toJsonRep date = { committer: { date } }
  fromJsonRep = _.committer.date

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
  , codec: CA.Record.object "PullRequestResponse"
      { url: CA.string
      }
  }

type PullRequest =
  { url :: String
  , state :: String
  , number :: Int
  , title :: String
  }

-- | Create a new pull request
requestGetPullRequests :: Address -> Request (Array PullRequest)
requestGetPullRequests address =
  { route: GitHubRoute GET [ "repos", address.owner, address.repo, "pulls" ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginate: true
  , codec: CA.array $ CA.Record.object "PullRequest"
      { url: CA.string
      , state: CA.string
      , number: CA.int
      , title: CA.string
      }
  }

type RateLimit =
  { limit :: Int
  , remaining :: Int
  , resetTime :: Maybe Instant
  }

requestRateLimit :: Request RateLimit
requestRateLimit =
  { route: GitHubRoute GET [ "rate_limit" ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginate: false
  , codec: Profunctor.dimap toJsonRep fromJsonRep $ CA.Record.object "RateLimit"
      { data: CA.Record.object "RateLimit.data"
          { resources: CA.Record.object "RateLimit.data.resources"
              { core: CA.Record.object "RateLimit.data.resources.core"
                  { limit: CA.int
                  , remaining: CA.int
                  , reset: CA.number
                  }
              }
          }
      }
  }
  where
  toJsonRep { limit, remaining, resetTime } = do
    let reset = Maybe.fromMaybe (-9999.0) ((unwrap <<< Instant.unInstant) <$> resetTime)
    { data: { resources: { core: { limit, remaining, reset } } } }

  fromJsonRep { data: { resources: { core: { limit, remaining, reset } } } } =
    { limit, remaining, resetTime: Instant.instant $ Aff.Milliseconds $ reset * 1000.0 }

-- | A route for the GitHub API, ie. "GET /repos/purescript/registry/tags".
-- | Meant for internal use.
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
-- | through the FFI. Should only be used with JavaScript-compatible types for
-- | the sake of setting headers.
data JSArgs

-- | Coerce a record to a JSArgs opaque type.
unsafeToJSArgs :: forall a. Record a -> JSArgs
unsafeToJSArgs = unsafeCoerce

noArgs :: JSArgs
noArgs = unsafeToJSArgs {}

type Request a =
  { route :: GitHubRoute
  , headers :: Object String
  , args :: JSArgs
  , paginate :: Boolean
  , codec :: JsonCodec a
  }

foreign import requestImpl :: forall r. EffectFn6 Octokit String (Object String) JSArgs (Object Json -> r) (Json -> r) (Promise r)
foreign import paginateImpl :: forall r. EffectFn6 Octokit String (Object String) JSArgs (Object Json -> r) (Json -> r) (Promise r)

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
    Right json -> case CA.decode codec json of
      Left decodeError -> Left $ DecodeError { error: CA.printJsonDecodeError decodeError, raw: Argonaut.stringifyWithIndent 2 json }
      Right parsed -> Right parsed
  where
  decodeGitHubAPIError :: Object Json -> Either String GitHubAPIError
  decodeGitHubAPIError object = lmap CA.printJsonDecodeError do
    statusCode <- atKey "status" CA.int object
    message <- case statusCode of
      304 -> pure ""
      500 -> pure "Internal server error"
      _ -> atKey "response" CA.jobject object >>= atKey "data" CA.jobject >>= atKey "message" CA.string
    pure { statusCode, message }

type GitHubAPIError =
  { statusCode :: Int
  , message :: String
  }

githubApiErrorCodec :: JsonCodec GitHubAPIError
githubApiErrorCodec = CA.Record.object "GitHubAPIError"
  { statusCode: CA.int
  , message: CA.string
  }

data GitHubError
  = UnexpectedError String
  | APIError GitHubAPIError
  | DecodeError { error :: String, raw :: String }

derive instance Eq GitHubError
derive instance Ord GitHubError

githubErrorCodec :: JsonCodec GitHubError
githubErrorCodec = Profunctor.dimap toVariant fromVariant $ CA.Variant.variantMatch
  { unexpectedError: Right CA.string
  , apiError: Right githubApiErrorCodec
  , decodeError: Right $ CA.Record.object "DecodeError"
      { error: CA.string
      , raw: CA.string
      }
  }
  where
  toVariant = case _ of
    UnexpectedError error -> Variant.inj (Proxy :: _ "unexpectedError") error
    APIError error -> Variant.inj (Proxy :: _ "apiError") error
    DecodeError error -> Variant.inj (Proxy :: _ "decodeError") error

  fromVariant = Variant.match
    { unexpectedError: UnexpectedError
    , apiError: APIError
    , decodeError: DecodeError
    }

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

atKey :: forall a. String -> JsonCodec a -> Object Json -> Either JsonDecodeError a
atKey key codec object =
  Maybe.maybe
    (Left (CA.AtKey key CA.MissingValue))
    (lmap (CA.AtKey key) <<< CA.decode codec)
    (Object.lookup key object)
