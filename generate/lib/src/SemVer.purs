module Lib.SemVer where

import Prelude

import Data.Argonaut.Core as Argonaut
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), note)
import Data.Either as Either
import Data.Int as Int
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.String as String
import Registry.Internal.Codec as Registry.Codec
import Registry.Version (Version)
import Registry.Version as Version
import Safe.Coerce (coerce)

newtype SemVer = SemVer { version :: Version, pre :: Maybe Int }

derive instance Newtype SemVer _
derive instance Eq SemVer

instance Ord SemVer where
  compare (SemVer l) (SemVer r) =
    case l.version `compare` r.version of
      -- We want to invert the normal Nothing < Just ordering of Maybe because
      -- the Just case represents a prerelease.
      EQ -> case l.pre, r.pre of
        Nothing, Nothing -> EQ
        Just _, Nothing -> LT
        Nothing, Just _ -> GT
        Just x, Just y -> x `compare` y
      x -> x

parse :: String -> Either String SemVer
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

print :: SemVer -> String
print (SemVer { pre, version }) = do
  let prerelease = maybe "" (\int -> "-" <> Int.toStringAs Int.decimal int) pre
  Version.print version <> prerelease

codec :: JsonCodec SemVer
codec = CA.codec' decode encode
  where
  encode = Argonaut.fromString <<< print <<< coerce
  decode = lmap CA.TypeMismatch <<< parse <=< CA.decode CA.string

semverMapCodec :: forall a. JsonCodec a -> JsonCodec (Map SemVer a)
semverMapCodec = Registry.Codec.strMap "SemVerMap" (Either.hush <<< parse) print
