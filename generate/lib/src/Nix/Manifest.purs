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
import Lib.Nix.System as NixSystem
import Lib.SemVer (SemVer)
import Lib.SemVer as SemVer
import Lib.Tool (ToolChannel, ToolPackage)
import Lib.Tool as Tool
import Registry.Internal.Codec as Registry.Codec
import Registry.Sha256 (Sha256)
import Registry.Sha256 as Sha256

type NamedManifest = Map ToolChannel ToolPackage

namedManifestCodec :: JsonCodec NamedManifest
namedManifestCodec = do
  let encodeKey = Tool.printToolChannel
  let decodeKey = Either.hush <<< Tool.parseToolChannel
  Registry.Codec.strMap "NamedManifest" decodeKey encodeKey Tool.toolPackageCodec

type SpagoManifest = Map SemVer (Either GitRev FetchUrl)

spagoManifestCodec :: JsonCodec SpagoManifest
spagoManifestCodec = SemVer.semverMapCodec (CA.codec' decode encode)
  where
  decode json =
    map Left (CA.decode gitRevCodec json)
      <|> map Right (CA.decode fetchUrlCodec json)

  encode = case _ of
    Left gitRev -> CA.encode gitRevCodec gitRev
    Right fetchUrl -> CA.encode fetchUrlCodec fetchUrl

type PursManifest = Map NixSystem (Map SemVer FetchUrl)

pursManifestCodec :: JsonCodec PursManifest
pursManifestCodec = do
  let encodeKey = NixSystem.print
  let decodeKey = Either.hush <<< NixSystem.parse
  Registry.Codec.strMap "PursManifest" decodeKey encodeKey (SemVer.semverMapCodec fetchUrlCodec)

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
