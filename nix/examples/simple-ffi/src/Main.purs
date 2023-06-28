module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

foreign import parseSpdx :: String -> String

main :: Effect Unit
main = do
  let spdx = "MIT"
  let parsed = parseSpdx spdx
  log parsed
