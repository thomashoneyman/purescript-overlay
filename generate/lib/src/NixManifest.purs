module Lib.NixManifest where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CA.Record
import Data.Either as Either
import Data.Map (Map)
import Lib.NixSystem (NixSystem)
import Lib.NixSystem as NixSystem
import Lib.NixVersion (NixVersion)
import Lib.NixVersion as NixVersion
import Registry.Internal.Codec as Registry.Codec
import Registry.Sha256 (Sha256)
import Registry.Sha256 as Sha256

type Manifests =
  { spago :: SpagoManifest
  , purs :: PursManifest
  }

type SpagoManifest = Map NixVersion SpagoManifestEntry

spagoManifestCodec :: JsonCodec SpagoManifest
spagoManifestCodec = NixVersion.nixVersionMapCodec spagoManifestEntryCodec

type SpagoManifestEntry = { rev :: String }

spagoManifestEntryCodec :: JsonCodec SpagoManifestEntry
spagoManifestEntryCodec = CA.Record.object "SpagoManifestEntry"
  { rev: CA.string
  }

type PursManifest = Map NixSystem (Map NixVersion PursManifestEntry)

pursManifestCodec :: JsonCodec PursManifest
pursManifestCodec = do
  let encodeKey = NixSystem.print
  let decodeKey = Either.hush <<< NixSystem.parse
  Registry.Codec.strMap "PursManifest" decodeKey encodeKey (NixVersion.nixVersionMapCodec pursManifestEntryCodec)

type PursManifestEntry = { url :: String, hash :: Sha256 }

pursManifestEntryCodec :: JsonCodec PursManifestEntry
pursManifestEntryCodec = CA.Record.object "PursManifestEntry"
  { url: CA.string
  , hash: Sha256.codec
  }
