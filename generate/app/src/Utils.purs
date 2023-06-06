module App.Utils where

import Prelude

import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Array as Array
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS.Aff
import Node.Path (FilePath)

readJsonFile :: forall a. FilePath -> JsonCodec a -> Aff a
readJsonFile path codec = do
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
