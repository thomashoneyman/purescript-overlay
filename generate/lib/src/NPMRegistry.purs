module Lib.NPMRegistry where

import Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJ.Common
import Data.Codec.JSON.Record as CJ.Record
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Profunctor as Profunctor
import Effect.Aff (Aff)
import Fetch as Fetch
import JSON as JSON
import JSON.Object as JSON.Object
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
    case JSON.parse response of
      Left parseErr ->
        pure $ Left $ "NPM response is not valid JSON: " <> parseErr <> "\n\n from raw text: " <> response
      Right json -> case CJ.decode npmPackageCodec json of
        Left decodeErr -> do
          let err = CJ.DecodeError.print decodeErr
          case JSON.toJObject json of
            Nothing -> pure $ Left $ "Failed to decode NPM response: " <> err <> "\n\n because NPM response is not an object in raw text: " <> response
            Just obj -> case JSON.Object.lookup "versions" obj of
              Nothing -> pure $ Left "NPM response has no 'versions' key!"
              Just versionsJson -> case JSON.toJObject versionsJson of
                Nothing -> pure $ Left "NPM response has a 'versions' key, but it's not an object!"
                Just versionsObject -> do
                  let versionKeys = JSON.Object.keys versionsObject
                  pure $ Left $ "Failed to decode NPM response (versions exists with " <> show (Array.length versionKeys) <> " keys: " <> show versionKeys <> "): " <> err
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

npmPackageCodec :: CJ.Codec NPMPackage
npmPackageCodec = CJ.Record.object
  { name: CJ.string
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

npmVersionCodec :: CJ.Codec NPMVersion
npmVersionCodec = CJ.Record.object
  { name: CJ.string
  , version: SemVer.codec
  , gitHead: CJ.Record.optional (Profunctor.wrapIso CommitSha CJ.string)
  , main: CJ.Record.optional CJ.string
  , dependencies: CJ.Record.optional (CJ.Common.strMap CJ.string)
  , dist: npmVersionDistCodec
  }

type NPMVersionDist =
  { tarball :: String
  }

npmVersionDistCodec :: CJ.Codec NPMVersionDist
npmVersionDistCodec = CJ.Record.object
  { tarball: CJ.string
  }
