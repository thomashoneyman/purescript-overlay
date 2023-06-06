module Lib.Nix.Version where

import Prelude

import Data.Argonaut.Core as Argonaut
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Registry.Internal.Codec as Registry.Codec
import Data.Either (Either(..), note)
import Data.Either as Either
import Data.Int as Int
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Registry.Version (Version)
import Registry.Version as Version
import Safe.Coerce (coerce)

newtype NixVersion = NixVersion { version :: Version, pre :: Maybe Int }

derive instance Eq NixVersion
derive instance Ord NixVersion

parse :: String -> Either String NixVersion
parse input = coerce $ case String.split (String.Pattern "-") input of
  [ raw ] -> do
    version <- Version.parse raw
    pure { version, pre: Nothing }
  [ raw, extra ] -> do
    version <- Version.parse raw
    pre <- note ("Failed to parse prerelease section as int: " <> extra) (Int.fromString extra)
    pure { version, pre: Just pre }
  other -> do
    Left $ "Expected version string split on '-' to have at maximum two components, but found more: " <> String.joinWith ", " other

print :: NixVersion -> String
print (NixVersion { pre, version }) = do
  let prerelease = maybe "" (\int -> "-" <> Int.toStringAs Int.decimal int) pre
  Version.print version <> prerelease

codec :: JsonCodec NixVersion
codec = CA.codec' decode encode
  where
  encode = Argonaut.fromString <<< print <<< coerce
  decode = lmap CA.TypeMismatch <<< parse <=< CA.decode CA.string

nixVersionMapCodec :: forall a. JsonCodec a -> JsonCodec (Map NixVersion a)
nixVersionMapCodec = Registry.Codec.strMap "NixVersionMap" (Either.hush <<< parse) print
