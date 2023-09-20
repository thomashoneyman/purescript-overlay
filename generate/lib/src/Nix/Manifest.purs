module Lib.Nix.Manifest where

import Prelude

import Control.Alt ((<|>))
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CA.Record
import Data.Either (Either(..))
import Data.Either as Either
import Data.Map (Map)
import Lib.Nix.System (NixSystem)
import Lib.Nix.System as Nix.System
import Lib.SemVer (SemVer)
import Lib.SemVer as SemVer
import Lib.Tool (Tool(..), ToolChannel, ToolPackage)
import Lib.Tool as Tool
import Node.Path (FilePath)
import Registry.Internal.Codec as Registry.Codec
import Registry.Sha256 (Sha256)
import Registry.Sha256 as Sha256

-- | The file name of the manifest for a given tool
filename :: Tool -> FilePath
filename = case _ of
  Purs -> "purs.json"
  Spago -> "spago.json"
  PursTidy -> "purs-tidy.json"
  PursBackendEs -> "purs-backend-es.json"
  PursLanguageServer -> "purescript-language-server.json"

type NamedManifest = Map ToolChannel ToolPackage

namedPath :: FilePath
namedPath = "named.json"

namedManifestCodec :: JsonCodec NamedManifest
namedManifestCodec = do
  let encodeKey = Tool.printToolChannel
  let decodeKey = Either.hush <<< Tool.parseToolChannel
  Registry.Codec.strMap "NamedManifest" decodeKey encodeKey Tool.toolPackageCodec

type PursManifest = Map SemVer (Map NixSystem FetchUrl)

pursManifestCodec :: JsonCodec PursManifest
pursManifestCodec = do
  let encodeKey = SemVer.print
  let decodeKey = Either.hush <<< SemVer.parse
  Registry.Codec.strMap "PursManifest" decodeKey encodeKey (Nix.System.systemMapCodec fetchUrlCodec)

type SpagoManifest = Map SemVer (Either FetchUrl (Map NixSystem FetchUrl))

spagoManifestCodec :: JsonCodec SpagoManifest
spagoManifestCodec = SemVer.semverMapCodec (CA.codec' decode encode)
  where
  decode json =
    map Left (CA.decode fetchUrlCodec json)
      <|> map Right (CA.decode (Nix.System.systemMapCodec fetchUrlCodec) json)
      <|> Left (CA.TypeMismatch "Expected a FetchUrl or a SystemMap FetchUrl")

  encode = case _ of
    Left gitRev -> CA.encode fetchUrlCodec gitRev
    Right fetchUrl -> CA.encode (Nix.System.systemMapCodec fetchUrlCodec) fetchUrl

type PursTidyManifest = Map SemVer FetchUrl

pursTidyManifestCodec :: JsonCodec PursTidyManifest
pursTidyManifestCodec = do
  let encodeKey = SemVer.print
  let decodeKey = Either.hush <<< SemVer.parse
  Registry.Codec.strMap "PursTidyManifest" decodeKey encodeKey fetchUrlCodec

type PursBackendEsManifest = Map SemVer FetchUrl

pursBackendEsManifestCodec :: JsonCodec PursBackendEsManifest
pursBackendEsManifestCodec = do
  let encodeKey = SemVer.print
  let decodeKey = Either.hush <<< SemVer.parse
  Registry.Codec.strMap "PursBackendEsManifest" decodeKey encodeKey fetchUrlCodec

type PursLanguageServerManifest = Map SemVer FetchUrl

pursLanguageServerManifestCodec :: JsonCodec PursLanguageServerManifest
pursLanguageServerManifestCodec = do
  let encodeKey = SemVer.print
  let decodeKey = Either.hush <<< SemVer.parse
  Registry.Codec.strMap "PursLanguageServerManifest" decodeKey encodeKey fetchUrlCodec

-- | A manifest entry for a package that can be fetched from git
type GitRev = { rev :: String }

gitRevCodec :: JsonCodec GitRev
gitRevCodec = CA.Record.object "SpagoManifestEntry"
  { rev: CA.string
  }

-- | A manifest entry for a package which has a fetchable tarball
type FetchUrl = { url :: String, hash :: Sha256 }

fetchUrlCodec :: JsonCodec FetchUrl
fetchUrlCodec = CA.Record.object "FetchUrl"
  { url: CA.string
  , hash: Sha256.codec
  }
