module Lib.Tool where

import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, enumFromTo)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)

-- | A tool that can be used to build a PureScript project.
data Tool
  = Purs
  | Spago
  | PursTidy
  | PursBackendEs

derive instance Eq Tool
derive instance Ord Tool
derive instance Generic Tool _

instance Enum Tool where
  pred = genericPred
  succ = genericSucc

instance Bounded Tool where
  bottom = genericBottom
  top = genericTop

instance BoundedEnum Tool where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

-- | Print a tool as its executable name
print :: Tool -> String
print = case _ of
  Purs -> "purs"
  Spago -> "spago"
  PursTidy -> "purs-tidy"
  PursBackendEs -> "purs-backend-es"

-- | Parse a tool from its executable name
parse :: String -> Either String Tool
parse = case _ of
  "purs" -> Right Purs
  "spago" -> Right Spago
  "purs-tidy" -> Right PursTidy
  "purs-backend-es" -> Right PursBackendEs
  other -> Left $ "Unknown tool: " <> other

-- | All tools supported by the library.
all :: Array Tool
all = enumFromTo bottom top
