module Lib.NPMRegistry where

import Prelude

import Data.Argonaut.Parser as Argonaut
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CA.Common
import Data.Codec.Argonaut.Record as CA.Record
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Fetch as Fetch
import Lib.SemVer (SemVer)
import Lib.SemVer as SemVer
import Lib.Tool (Tool(..))
import Node.Path (FilePath)

-- NOTE: Spago bundles all but better-sqlite3 in their release process,
-- so I need to handle that somehow. Like some kind of 'extra_node_modules'
-- I can apply.

listNPMReleases :: Tool -> Aff (Either String (Map SemVer NPMVersion))
listNPMReleases tool = do
  { status, text } <- Fetch.fetch (printNPMRegistryUrl tool) {}
  response <- text
  if status /= 200 then do
    pure $ Left $ "Received non-200 status in request to NPM registry: " <> response
  else do
    case (lmap CA.printJsonDecodeError <<< CA.decode npmPackageCodec) =<< Argonaut.jsonParser response of
      Left err -> do
        pure $ Left $ "Failed to decode NPM response: " <> err <> "\n\n from raw text: " <> response
      Right npmPackage ->
        pure $ Right $ npmPackage.versions

printNPMRegistryUrl :: Tool -> String
printNPMRegistryUrl = case _ of
  Purs -> "https://registry.npmjs.org/purescript"
  Spago -> "https://registry.npmjs.org/spago"
  PursTidy -> "https://registry.npmjs.org/purs-tidy"
  PursBackendEs -> "https://registry.npmjs.org/purs-backend-es"
  PursLanguageServer -> "https://registry.npmjs.org/purescript-language-server"

type NPMPackage =
  { name :: String
  , versions :: Map SemVer NPMVersion
  }

npmPackageCodec :: JsonCodec NPMPackage
npmPackageCodec = CA.Record.object "NPMPackage"
  { name: CA.string
  , versions: SemVer.semverMapCodec npmVersionCodec
  }

type NPMVersion =
  { name :: String
  , version :: SemVer
  , main :: Maybe FilePath
  , dependencies :: Maybe (Map String String)
  , dist :: NPMVersionDist
  }

npmVersionCodec :: JsonCodec NPMVersion
npmVersionCodec = CA.Record.object "NPMVersion"
  { name: CA.string
  , version: SemVer.codec
  , main: CA.Record.optional CA.string
  , dependencies: CA.Record.optional (CA.Common.strMap CA.string)
  , dist: npmVersionDistCodec
  }

type NPMVersionDist =
  { tarball :: String
  }

npmVersionDistCodec :: JsonCodec NPMVersionDist
npmVersionDistCodec = CA.Record.object "NPMVersionDist"
  { tarball: CA.string
  }
