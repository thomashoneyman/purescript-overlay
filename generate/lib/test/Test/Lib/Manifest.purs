module Test.Lib.Manifest where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.String as String
import Data.Traversable (for)
import Lib.Nix.Manifest (NamedManifest, PursManifest, SpagoManifest, namedManifestCodec, pursManifestCodec, spagoManifestCodec)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Utils as Utils

spec :: Spec Unit
spec = do
  Spec.it "Round-trips manifest fixtures" do
    let manifestFixturesPath = Path.concat [ "..", "..", "manifests" ]
    paths <- FS.Aff.readdir manifestFixturesPath
    let fixturePaths = Array.filter (\path -> Path.extname path == ".json") paths
    fixtures <- for fixturePaths \path -> do
      rawManifest <- FS.Aff.readTextFile UTF8 $ Path.concat [ manifestFixturesPath, path ]
      pure { label: path, value: String.trim rawManifest }
    Utils.shouldRoundTrip "Manifest" nixManifestCodec fixtures

data NixManifest
  = SpagoManifest SpagoManifest
  | PursManifest PursManifest
  | NamedManifest NamedManifest

derive instance Eq NixManifest

nixManifestCodec :: JsonCodec NixManifest
nixManifestCodec = CA.codec' decode encode
  where
  encode = case _ of
    SpagoManifest manifest -> CA.encode spagoManifestCodec manifest
    PursManifest manifest -> CA.encode pursManifestCodec manifest
    NamedManifest manifest -> CA.encode namedManifestCodec manifest

  decode json =
    map SpagoManifest (CA.decode spagoManifestCodec json)
      <|> map PursManifest (CA.decode pursManifestCodec json)
      <|> map NamedManifest (CA.decode namedManifestCodec json)
