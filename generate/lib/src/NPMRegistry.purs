module Lib.NPMRegistry where

import Prelude

import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CA.Common
import Data.Codec.Argonaut.Record as CA.Record
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Profunctor as Profunctor
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Fetch as Fetch
import Foreign.Object as Object
import Lib.Git (CommitSha(..))
import Lib.SemVer (SemVer)
import Lib.SemVer as SemVer
import Lib.Tool (Tool(..))
import Node.Path (FilePath)

listNPMReleases :: Tool -> Aff (Either String (Map SemVer NPMVersion))
listNPMReleases tool = do
  { status, text } <- Fetch.fetch (printNPMRegistryUrl tool) {}
  response <- text
  if status /= 200 then do
    pure $ Left $ "Received non-200 status in request to NPM registry: " <> response
  else do
    case Argonaut.Parser.jsonParser response of
      Left parseErr ->
        pure $ Left $ "NPM response is not valid JSON: " <> parseErr <> "\n\n from raw text: " <> response
      Right json -> case CA.decode npmPackageCodec json of
        Left decodeErr -> do
          let err = CA.printJsonDecodeError decodeErr
          case Argonaut.toObject json of
            Nothing -> pure $ Left $ "Failed to decode NPM response: " <> err <> "\n\n because NPM response is not an object in raw text: " <> response
            Just obj -> case Object.lookup "versions" obj of
              Nothing -> pure $ Left "NPM response has no 'versions' key!"
              Just versionsJson -> case Argonaut.toObject versionsJson of
                Nothing -> pure $ Left "NPM response has a 'versions' key, but it's not an object!"
                Just versionsObject -> case traverse (CA.decode npmVersionCodec) versionsObject of
                  Left decodeErr2 -> pure $ Left $ "Failed to decode NPM response: " <> CA.printJsonDecodeError decodeErr2
                  Right _ -> pure $ Left $ "Failed to decode NPM response (versions OK): " <> err <> "\n\n " <> Argonaut.stringify (Argonaut.fromObject (Object.delete "versions" obj))
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
  , gitHead :: Maybe CommitSha
  , dependencies :: Maybe (Map String String)
  , dist :: NPMVersionDist
  }

npmVersionCodec :: JsonCodec NPMVersion
npmVersionCodec = CA.Record.object "NPMVersion"
  { name: CA.string
  , version: SemVer.codec
  , gitHead: CA.Record.optional (Profunctor.wrapIso CommitSha CA.string)
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
