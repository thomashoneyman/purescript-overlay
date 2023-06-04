module App.Main where

import Prelude

import Data.Either as Either
import Effect (Effect)
import Effect.Console (log)
import Lib as Lib
import Registry.License as License

main :: Effect Unit
main = log $ Lib.pizza <> " " <> Either.either identity License.print (License.parse "MIT")
