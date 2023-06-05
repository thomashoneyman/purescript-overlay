module Lib.NixSystem where

import Prelude

import Data.Argonaut.Core as Argonaut
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Either as Either
import Data.Map (Map)
import Data.String as String
import Registry.Internal.Codec as Registry.Codec

data NixSystem = X86_64_linux | X86_64_darwin | AARCH_64_darwin

derive instance Eq NixSystem
derive instance Ord NixSystem

parse :: String -> Either String NixSystem
parse input = case input of
  "x86_64-linux" -> Right X86_64_linux
  "x86_64-darwin" -> Right X86_64_darwin
  "aarch64-darwin" -> Right AARCH_64_darwin
  other -> do
    let members = [ "x86_64-linux", "x86_64-darwin", "aarch64-darwin" ]
    Left $ "Expected one of " <> String.joinWith ", " members <> " but saw: " <> other

print :: NixSystem -> String
print = case _ of
  X86_64_linux -> "x86_64-linux"
  X86_64_darwin -> "x86_64-darwin"
  AARCH_64_darwin -> "aarch64-darwin"

codec :: JsonCodec NixSystem
codec = CA.codec' decode encode
  where
  encode = Argonaut.fromString <<< print
  decode = lmap CA.TypeMismatch <<< parse <=< CA.decode CA.string

nixSystemMapCodec :: forall a. JsonCodec a -> JsonCodec (Map NixSystem a)
nixSystemMapCodec = Registry.Codec.strMap "NixSystemMap" (Either.hush <<< parse) print
