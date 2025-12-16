module Lib.Nix.Manifest where

import Prelude

import Control.Alt ((<|>))
import Data.Codec as Codec
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJ.Record
import Data.Either (Either(..))
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

namedManifestCodec :: CJ.Codec NamedManifest
namedManifestCodec = do
  let encodeKey = Tool.printToolChannel
  let decodeKey = Tool.parseToolChannel
  Registry.Codec.strMap "NamedManifest" decodeKey encodeKey Tool.toolPackageCodec

type GitHubBinaryManifest = Map SemVer (Map NixSystem FetchUrl)

githubBinaryManifestCodec :: CJ.Codec GitHubBinaryManifest
githubBinaryManifestCodec = do
  let encodeKey = SemVer.print
  let decodeKey = SemVer.parse
  Registry.Codec.strMap "GitHubBinaryManifest" decodeKey encodeKey (Nix.System.systemMapCodec fetchUrlCodec)

type NPMRegistryManifest = Map SemVer NPMFetch

npmRegistryManifestCodec :: CJ.Codec NPMRegistryManifest
npmRegistryManifestCodec = do
  let encodeKey = SemVer.print
  let decodeKey = SemVer.parse
  Registry.Codec.strMap "NPMRegistryManifest" decodeKey encodeKey npmFetchCodec

type CombinedManifest = Map SemVer (Either NPMFetch (Map NixSystem FetchUrl))

combinedManifestCodec :: CJ.Codec CombinedManifest
combinedManifestCodec = SemVer.semverMapCodec (Codec.codec' decode encode)
  where
  decode json =
    map Left (Codec.decode npmFetchCodec json)
      <|> map Right (Codec.decode (Nix.System.systemMapCodec fetchUrlCodec) json)

  encode = case _ of
    Left npmFetch -> Codec.encode npmFetchCodec npmFetch
    Right fetchUrl -> Codec.encode (Nix.System.systemMapCodec fetchUrlCodec) fetchUrl

-- | A manifest entry for a package which has a fetchable tarball, where the
-- | tarball contains either the entire bundled script, or contains a script
-- | that still requires dependencies. Since the package-lock.json is not
-- | included in the NPM tarball we fetch it separately from GitHub.
data NPMFetch
  = Bundled FetchUrl
  | Unbundled FetchWithLock
  | UnbundledLocal FetchWithPath

derive instance Eq NPMFetch

npmFetchCodec :: CJ.Codec NPMFetch
npmFetchCodec = Codec.codec' decode encode
  where
  decode json =
    map Bundled (Codec.decode fetchUrlCodec json)
      <|> map Unbundled (Codec.decode fetchWithLockCodec json)
      <|> map UnbundledLocal (Codec.decode fetchWithPathCodec json)

  encode = case _ of
    Bundled fetchUrl -> Codec.encode fetchUrlCodec fetchUrl
    Unbundled fetchWithLock -> Codec.encode fetchWithLockCodec fetchWithLock
    UnbundledLocal fetchWithPath -> Codec.encode fetchWithPathCodec fetchWithPath

type FetchWithPath = { tarball :: FetchUrl, path :: FilePath, depsHash :: Sha256 }

fetchWithPathCodec :: CJ.Codec FetchWithPath
fetchWithPathCodec = CJ.Record.object
  { tarball: fetchUrlCodec
  , path: CJ.string
  , depsHash: Sha256.codec
  }

type FetchWithLock = { tarball :: FetchUrl, lockfile :: FetchUrl, depsHash :: Sha256 }

fetchWithLockCodec :: CJ.Codec FetchWithLock
fetchWithLockCodec = CJ.Record.object
  { tarball: fetchUrlCodec
  , lockfile: fetchUrlCodec
  , depsHash: Sha256.codec
  }

-- | A manifest entry for a package which has a fetchable tarball
type FetchUrl = { url :: String, hash :: Sha256 }

fetchUrlCodec :: CJ.Codec FetchUrl
fetchUrlCodec = CJ.Record.object
  { url: CJ.string
  , hash: Sha256.codec
  }
