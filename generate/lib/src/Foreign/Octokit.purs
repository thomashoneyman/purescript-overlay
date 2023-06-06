-- | Low-level bindings to Octokit and its request functions.
module Lib.Foreign.Octokit
  ( Address
  , GitHubAPIError
  , GitHubError(..)
  , GitHubRoute
  , JSArgs
  , Octokit
  , RateLimit
  , Release
  , ReleaseAsset
  , Request
  , newOctokit
  , requestListReleases
  , requestGetReleaseByTagName
  , requestGetRefCommitSha
  , requestGetCommitDate
  , requestCreatePullRequest
  , requestRateLimit
  , request
  , printGitHubError
  , githubErrorCodec
  , releaseCodec
  , releaseAssetCodec
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
import Data.Newtype (unwrap)
import Data.Profunctor as Profunctor
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn6, runEffectFn6)
import Foreign.Object (Object)
import Foreign.Object as Object
import Registry.Internal.Codec as Internal.Codec
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | An instance of GitHub's Octokit client
foreign import data Octokit :: Type

foreign import newOctokitImpl :: Effect Octokit

newOctokit :: forall m. MonadEffect m => m Octokit
newOctokit = liftEffect newOctokitImpl

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
  }

releaseCodec :: JsonCodec Release
releaseCodec = Profunctor.dimap toJsonRep fromJsonRep $ CA.Record.object "Release"
  { html_url: CA.string
  , tag_name: CA.string
  , draft: CA.boolean
  , prerelease: CA.boolean
  , assets: CA.array releaseAssetCodec
  }
  where
  toJsonRep { url, tag, draft, prerelease, assets } =
    { html_url: url
    , tag_name: tag
    , draft
    , prerelease
    , assets
    }

  fromJsonRep { html_url, tag_name, draft, prerelease, assets } =
    { url: html_url
    , tag: tag_name
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
requestCreatePullRequest :: { address :: Address, content :: PullContent } -> Request Unit
requestCreatePullRequest { address, content: { title, body, head, base } } =
  { route: GitHubRoute POST [ "repos", address.owner, address.repo, "pulls" ] Map.empty
  , headers: Object.empty
  , args: unsafeToJSArgs { title, body, head, base }
  , paginate: false
  , codec: CA.codec' (\_ -> pure unit) (CA.encode CA.null)
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
  result <- liftAff $ Promise.toAffE $ runEffectFn6 (if paginate then paginateImpl else requestImpl) octokit (printGitHubRoute route) headers args Left Right
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
