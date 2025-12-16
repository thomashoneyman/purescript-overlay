module Test.Utils where

import Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.Either (Either(..))
import Data.String as String
import Effect.Exception (Error)
import JSON as JSON
import Lib.Utils as App.Utils
import Test.Spec.Assertions as Assert

type Fixture = { label :: String, value :: String }

-- | Round-trip an input JSON string
shouldRoundTrip :: forall m a. MonadThrow Error m => String -> CJ.Codec a -> Array Fixture -> m Unit
shouldRoundTrip ty codec fixtures = do
  let
    parseFixture { label, value } =
      case JSON.parse value of
        Left error -> Left { label, input: value, error }
        Right json -> case CJ.decode codec json of
          Left error -> Left { label, input: value, error: CJ.DecodeError.print error }
          Right result -> Right { label, input: value, result }

    fixtureParseResult = App.Utils.partitionEithers (map parseFixture fixtures)

    formatFixtureError { label, input, error } = label <> " failed with " <> error <> " for input:\n" <> input

  unless (Array.null fixtureParseResult.fail) do
    Assert.fail $ String.joinWith "\n"
      [ "Some well-formed " <> ty <> " strings were not parsed correctly:"
      , Array.foldMap (append "\n  - " <<< formatFixtureError) fixtureParseResult.fail
      ]

  let
    roundtrip = fixtureParseResult.success <#> \fields -> do
      let printed = JSON.printIndented $ CJ.encode codec fields.result
      let input = String.trim fields.input
      if input == printed then Right unit else Left { label: fields.label, input, printed }

    roundtripResult = App.Utils.partitionEithers roundtrip

    formatRoundtripError { label, input, printed } =
      String.joinWith "\n"
        [ label <> " input does not match output."
        , String.joinWith "\n" [ input, "/=", printed ]
        ]

  unless (Array.null roundtripResult.fail) do
    Assert.fail $ String.joinWith "\n"
      [ "Some well-formed " <> ty <> " did not round-trip:"
      , Array.foldMap (append "\n  - " <<< formatRoundtripError) roundtripResult.fail
      ]
