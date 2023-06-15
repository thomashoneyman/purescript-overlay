module Lib.Tool where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, enumFromTo)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.String as String
import Data.Tuple (Tuple(..))
import Lib.SemVer (SemVer(..))
import Registry.Version as Version

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

-- | All tools supported by the library.
all :: Array Tool
all = enumFromTo bottom top

-- | How to format a tool when printing or parsing
data Format
  = Executable
  | NixChannel Channel
  | NixPackage SemVer

derive instance Eq Format

-- | A mutable reference for a tool indicating its latest stable or unstable
-- | version
data Channel = Stable | Unstable

derive instance Eq Channel
derive instance Ord Channel

printChannel :: Channel -> String
printChannel = case _ of
  Stable -> "stable"
  Unstable -> "unstable"

parseChannel :: String -> Either String Channel
parseChannel = case _ of
  "stable" -> Right Stable
  "unstable" -> Right Unstable
  _ -> Left "Expected 'stable' or 'unstable'"

-- | Print a tool according to the specified format:
-- |   - executable -> "purs"
-- |   - channel -> "purs-stable"
-- |   - package -> "purs-0_14_4-0"
print :: Tool -> Format -> String
print tool = case _ of
  Executable -> printExecutable tool
  NixChannel channel -> printToolChannel $ ToolChannel { tool, channel }
  NixPackage version -> printToolPackage $ ToolPackage { tool, version }

-- | Parse a tool from one of the available formats
parse :: String -> Either String (Tuple Tool Format)
parse str =
  map (flip Tuple Executable) (parseExecutable str)
    <|> map (\(ToolChannel { tool, channel }) -> Tuple tool (NixChannel channel)) (parseToolChannel str)
    <|> map (\(ToolPackage { tool, version }) -> Tuple tool (NixPackage version)) (parseToolPackage str)
    <|> Left ("Expected a tool name in the form 'tool', 'tool-channel' or 'tool-X_Y_Z' but got: " <> str)

-- | Print a tool as its executable name, ie. 'purs'
printExecutable :: Tool -> String
printExecutable = case _ of
  Purs -> "purs"
  Spago -> "spago"
  PursTidy -> "purs-tidy"
  PursBackendEs -> "purs-backend-es"

-- | Parse a tool from its executable name, ie. 'purs'
parseExecutable :: String -> Either String Tool
parseExecutable = case _ of
  "purs" -> Right Purs
  "spago" -> Right Spago
  "purs-tidy" -> Right PursTidy
  "purs-backend-es" -> Right PursBackendEs
  other -> Left $ "Unknown tool: " <> other

-- | A tool and its channel, ie. 'purs-stable'
newtype ToolChannel = ToolChannel { tool :: Tool, channel :: Channel }

derive instance Newtype ToolChannel _
derive newtype instance Eq ToolChannel

-- The derived instance will compare field-by-field alphabetically, but we
-- want to force the tool to be first.
instance Ord ToolChannel where
  compare (ToolChannel a) (ToolChannel b) =
    case compare a.tool b.tool of
      EQ -> compare a.channel b.channel
      ordering -> ordering

-- | Print a tool and its channel, ie. 'purs-stable'
printToolChannel :: ToolChannel -> String
printToolChannel (ToolChannel { tool, channel }) = printExecutable tool <> "-" <> printChannel channel

-- | Parse a tool from its executable name and channel, ie. 'purs-stable'
parseToolChannel :: String -> Either String ToolChannel
parseToolChannel str = case String.lastIndexOf (String.Pattern "-") str of
  Nothing -> Left $ "Expected a tool name in the form 'tool-channel' but got: " <> str
  Just index -> do
    let toolStr = String.take index str
    let channelStr = String.drop (index + 1) str
    tool <- parseExecutable toolStr
    channel <- parseChannel channelStr
    pure $ ToolChannel { tool, channel }

toolChannelCodec :: JsonCodec ToolChannel
toolChannelCodec = CA.codec' decode encode
  where
  decode json = do
    str <- CA.decode CA.string json
    lmap CA.TypeMismatch $ parseToolChannel str

  encode = CA.encode CA.string <<< printToolChannel

-- | A tool and its version, ie. 'purs-0_14_4-0'
newtype ToolPackage = ToolPackage { tool :: Tool, version :: SemVer }

derive instance Newtype ToolPackage _
derive newtype instance Eq ToolPackage

-- The derived instance will compare field-by-field alphabetically, but we
-- want to force the tool to be first.
instance Ord ToolPackage where
  compare (ToolPackage a) (ToolPackage b) =
    case compare a.tool b.tool of
      EQ -> compare a.version b.version
      ordering -> ordering

printToolPackage :: ToolPackage -> String
printToolPackage (ToolPackage { tool, version: SemVer { version, pre } }) = Array.fold
  [ printExecutable tool
  , "-"
  , String.replaceAll (String.Pattern ".") (String.Replacement "_") (Version.print version)
  , pre # maybe "" \int -> "-" <> show int
  ]

-- | Parse a tool from its executable name and version as a package, ie.
-- | 'purs-0_14_4-0'
parseToolPackage :: String -> Either String ToolPackage
parseToolPackage str = case String.split (String.Pattern "-") str of
  [ toolStr, versionStr ] -> do
    tool <- parseExecutable toolStr
    version <- Version.parse (unformatVersion versionStr)
    pure $ ToolPackage { tool, version: SemVer { version, pre: Nothing } }
  [ toolStr, versionStr, preStr ] -> do
    tool <- parseExecutable toolStr
    version <- Version.parse (unformatVersion versionStr)
    pre <- case preStr of
      "" -> pure Nothing
      _ -> case Int.fromString preStr of
        Nothing -> Left $ "Expected a number for the pre-release part of the version, but got: " <> preStr
        Just int -> pure $ Just int
    pure $ ToolPackage { tool, version: SemVer { version, pre } }
  _ -> Left $ "Expected a tool name in the form 'tool-X_Y_Z' or 'tool-X_Y_Z-pre' but got: " <> str
  where
  unformatVersion = String.replaceAll (String.Pattern "_") (String.Replacement ".")

toolPackageCodec :: JsonCodec ToolPackage
toolPackageCodec = CA.codec' decode encode
  where
  decode json = do
    str <- CA.decode CA.string json
    lmap CA.TypeMismatch $ parseToolPackage str

  encode = CA.encode CA.string <<< printToolPackage
