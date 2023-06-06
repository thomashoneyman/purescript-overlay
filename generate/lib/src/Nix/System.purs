module Lib.Nix.System where

import Prelude

import Data.Argonaut.Core as Argonaut
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Either as Either
import Data.Map (Map)
import Data.String as String
import Registry.Internal.Codec as Registry.Codec

data NixSystem
  = X86_64_linux
  | X86_64_darwin
  | AARCH_64_linux
  | AARCH_64_darwin

derive instance Eq NixSystem
derive instance Ord NixSystem

parse :: String -> Either String NixSystem
parse input = case input of
  "x86_64-linux" -> Right X86_64_linux
  "x86_64-darwin" -> Right X86_64_darwin
  "aarch64-linux" -> Right AARCH_64_linux
  "aarch64-darwin" -> Right AARCH_64_darwin
  other -> do
    let members = [ "x86_64-linux", "x86_64-darwin", "aarch64-linux", "aarch64-darwin" ]
    Left $ "Expected one of " <> String.joinWith ", " members <> " but saw: " <> other

print :: NixSystem -> String
print = case _ of
  X86_64_linux -> "x86_64-linux"
  X86_64_darwin -> "x86_64-darwin"
  AARCH_64_linux -> "aarch64-linux"
  AARCH_64_darwin -> "aarch64-darwin"

codec :: JsonCodec NixSystem
codec = CA.codec' decode encode
  where
  encode = Argonaut.fromString <<< print
  decode = lmap CA.TypeMismatch <<< parse <=< CA.decode CA.string

nixSystemMapCodec :: forall a. JsonCodec a -> JsonCodec (Map NixSystem a)
nixSystemMapCodec = Registry.Codec.strMap "NixSystemMap" (Either.hush <<< parse) print

-- | Parse the name of a PureScript release tarball into a Nix system
fromPursReleaseTarball :: String -> Either String NixSystem
fromPursReleaseTarball assetName = do
  name <- Either.note ("Expected .tar.gz suffix: " <> assetName) (String.stripSuffix (String.Pattern ".tar.gz") assetName)

  let
    aarch64_linux = [ "linux-arm64" ]
    x86_64_linux = [ "linux64" ]
    aarch64_darwin = [ "macos-arm64" ]
    x86_64_darwin = [ "macos" ]

  if Array.elem name aarch64_linux then
    Right AARCH_64_linux
  else if Array.elem name x86_64_linux then
    Right X86_64_linux
  else if Array.elem name aarch64_darwin then
    Right AARCH_64_darwin
  else if Array.elem name x86_64_darwin then
    Right X86_64_darwin
  else
    Left $ "Could not determine which Nix system should be assigned to: " <> name
