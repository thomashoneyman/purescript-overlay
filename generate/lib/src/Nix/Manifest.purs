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

type GitHubBinaryManifest = Map SemVer (Map NixSystem FetchUrl)

githubBinaryManifestCodec :: JsonCodec GitHubBinaryManifest
githubBinaryManifestCodec = do
  let encodeKey = SemVer.print
  let decodeKey = Either.hush <<< SemVer.parse
  Registry.Codec.strMap "GitHubBinaryManifest" decodeKey encodeKey (Nix.System.systemMapCodec fetchUrlCodec)

type NPMRegistryManifest = Map SemVer NPMFetch

npmRegistryManifestCodec :: JsonCodec NPMRegistryManifest
npmRegistryManifestCodec = do
  let encodeKey = SemVer.print
  let decodeKey = Either.hush <<< SemVer.parse
  Registry.Codec.strMap "NPMRegistryManifest" decodeKey encodeKey npmFetchCodec

type CombinedManifest = Map SemVer (Either NPMFetch (Map NixSystem FetchUrl))

combinedManifestCodec :: JsonCodec CombinedManifest
combinedManifestCodec = SemVer.semverMapCodec (CA.codec' decode encode)
  where
  decode json =
    map Left (CA.decode npmFetchCodec json)
      <|> map Right (CA.decode (Nix.System.systemMapCodec fetchUrlCodec) json)
      <|> Left (CA.TypeMismatch "Expected a FetchUrl or a SystemMap FetchUrl")

  encode = case _ of
    Left npmFetch -> CA.encode npmFetchCodec npmFetch
    Right fetchUrl -> CA.encode (Nix.System.systemMapCodec fetchUrlCodec) fetchUrl

-- | A manifest entry for a package which has a fetchable tarball, where the
-- | tarball contains either the entire bundled script, or contains a script
-- | that still requires dependencies. Since the package-lock.json is not
-- | included in the NPM tarball we fetch it separately from GitHub.
data NPMFetch
  = Bundled FetchUrl
  | Unbundled FetchUrlAndLock

derive instance Eq NPMFetch

npmFetchCodec :: JsonCodec NPMFetch
npmFetchCodec = CA.codec' decode encode
  where
  decode json =
    map Bundled (CA.decode fetchUrlCodec json)
      <|> map Unbundled (CA.decode fetchUrlAndLockCodec json)
      <|> Left (CA.TypeMismatch "Expected a FetchUrl or a FetchUrlAndLock")

  encode = case _ of
    Bundled fetchUrl -> CA.encode fetchUrlCodec fetchUrl
    Unbundled fetchUrlAndLock -> CA.encode fetchUrlAndLockCodec fetchUrlAndLock

type FetchUrlAndLock = { tarball :: FetchUrl, lockfile :: FilePath }

fetchUrlAndLockCodec :: JsonCodec FetchUrlAndLock
fetchUrlAndLockCodec = CA.Record.object "FetchUrlAndLock"
  { tarball: fetchUrlCodec
  , lockfile: CA.string
  }

-- | A manifest entry for a package which has a fetchable tarball
type FetchUrl = { url :: String, hash :: Sha256 }

fetchUrlCodec :: JsonCodec FetchUrl
fetchUrlCodec = CA.Record.object "FetchUrl"
  { url: CA.string
  , hash: Sha256.codec
  }
