module Test.Lib.Manifest where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.String as String
import Data.Traversable (for)
import Lib.Nix.Manifest (CombinedManifest, GitHubBinaryManifest, NPMRegistryManifest, NamedManifest, combinedManifestCodec, githubBinaryManifestCodec, namedManifestCodec, npmRegistryManifestCodec)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Utils as Utils

spec :: Spec Unit
spec = do
  Spec.it "Round-trips manifest fixtures" do
    let manifestFixturesPath = Path.concat [ "..", "manifests" ]
    paths <- FS.Aff.readdir manifestFixturesPath
    let fixturePaths = Array.filter (\path -> Path.extname path == ".json") paths
    fixtures <- for fixturePaths \path -> do
      rawManifest <- FS.Aff.readTextFile UTF8 $ Path.concat [ manifestFixturesPath, path ]
      pure { label: path, value: String.trim rawManifest }
    Utils.shouldRoundTrip "Manifest" nixManifestCodec fixtures

data NixManifest
  = SpagoManifest CombinedManifest
  | PursManifest GitHubBinaryManifest
  | PursTidyManifest NPMRegistryManifest
  | PursBackendEsManifest NPMRegistryManifest
  | PursLanguageServerManifest NPMRegistryManifest
  | NamedManifest NamedManifest

derive instance Eq NixManifest

nixManifestCodec :: JsonCodec NixManifest
nixManifestCodec = CA.codec' decode encode
  where
  encode = case _ of
    SpagoManifest manifest -> CA.encode combinedManifestCodec manifest
    PursManifest manifest -> CA.encode githubBinaryManifestCodec manifest
    PursTidyManifest manifest -> CA.encode npmRegistryManifestCodec manifest
    PursBackendEsManifest manifest -> CA.encode npmRegistryManifestCodec manifest
    PursLanguageServerManifest manifest -> CA.encode npmRegistryManifestCodec manifest
    NamedManifest manifest -> CA.encode namedManifestCodec manifest

  decode json =
    map SpagoManifest (CA.decode combinedManifestCodec json)
      <|> map PursManifest (CA.decode githubBinaryManifestCodec json)
      <|> map PursTidyManifest (CA.decode npmRegistryManifestCodec json)
      <|> map PursBackendEsManifest (CA.decode npmRegistryManifestCodec json)
      <|> map PursLanguageServerManifest (CA.decode npmRegistryManifestCodec json)
      <|> map NamedManifest (CA.decode namedManifestCodec json)
      <|> Left (CA.TypeMismatch "Expected a CombinedManifest, GitHubBinaryManifest, NPMRegistryManifest, or NamedManifest")
