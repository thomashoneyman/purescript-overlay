module Lib.Utils where

import Prelude

import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Array as Array
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Foldable as Foldable
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff as Parallel
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS.Aff
import Node.Path (FilePath)
import Node.Process as Process

writeJsonFile :: forall m a. MonadAff m => FilePath -> JsonCodec a -> a -> m Unit
writeJsonFile path codec a = liftAff do
  let encoded = CA.encode codec a
  let text = Argonaut.stringifyWithIndent 2 encoded
  FS.Aff.writeTextFile UTF8 path (text <> "\n")

readJsonFile :: forall m a. MonadAff m => FilePath -> JsonCodec a -> m a
readJsonFile path codec = liftAff do
  text <- FS.Aff.readTextFile UTF8 path
  json <- case Argonaut.Parser.jsonParser text of
    Left error -> Aff.throwError $ Aff.error error
    Right json -> pure json
  decoded <- case CA.decode codec json of
    Left error -> Aff.throwError $ Aff.error $ CA.printJsonDecodeError error
    Right value -> pure value
  pure decoded

-- | Partition an array of `Either` values into failure and success  values
partitionEithers :: forall e a. Array (Either e a) -> { fail :: Array e, success :: Array a }
partitionEithers = Array.foldMap case _ of
  Left err -> { fail: [ err ], success: [] }
  Right res -> { fail: [], success: [ res ] }

-- | Print a type as a formatted JSON string
printJson :: forall a. JsonCodec a -> a -> String
printJson codec = Argonaut.stringifyWithIndent 2 <<< CA.encode codec

-- | Attempt an effectful computation with exponential backoff.
withBackoff' :: forall m a. MonadAff m => Aff a -> m (Maybe a)
withBackoff' action = liftAff $ withBackoff
  { delay: Aff.Milliseconds 5_000.0
  , action
  , shouldCancel: \_ -> pure true
  , shouldRetry: \attempt -> if attempt > 3 then pure Nothing else pure (Just action)
  }

type Backoff a =
  { delay :: Aff.Milliseconds
  , action :: Aff a
  , shouldCancel :: Int -> Aff Boolean
  , shouldRetry :: Int -> Aff (Maybe (Aff a))
  }

-- | Attempt an effectful computation with exponential backoff, starting with
-- | the provided timeout.
withBackoff :: forall m a. MonadAff m => Backoff a -> m (Maybe a)
withBackoff { delay: Aff.Milliseconds timeout, action, shouldCancel, shouldRetry } = liftAff do
  let
    runAction attempt action' ms =
      Parallel.sequential $ Foldable.oneOf
        [ Parallel.parallel (map Just action')
        , Parallel.parallel (runTimeout attempt ms)
        ]

    runTimeout attempt ms = do
      _ <- Aff.delay (Aff.Milliseconds (Int.toNumber ms))
      shouldCancel attempt >>= if _ then pure Nothing else runTimeout attempt (ms * 2)

    loop :: Int -> Maybe a -> Aff (Maybe a)
    loop attempt = case _ of
      Nothing -> do
        maybeRetry <- shouldRetry attempt
        case maybeRetry of
          Nothing -> pure Nothing
          Just newAction -> do
            let newTimeout = Int.floor timeout `Int.pow` (attempt + 1)
            maybeResult <- runAction attempt newAction newTimeout
            loop (attempt + 1) maybeResult
      Just result ->
        pure (Just result)

  maybeResult <- runAction 0 action (Int.floor timeout)
  loop 1 maybeResult

die :: forall m u. MonadEffect m => String -> m u
die msg = do
  Console.error msg
  liftEffect (Process.exit' 1)