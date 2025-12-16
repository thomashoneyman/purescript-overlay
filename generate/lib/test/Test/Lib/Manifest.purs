module Test.Lib.Manifest where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (except)
import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Array as Array
import Data.Codec as Codec
import Data.Codec.JSON as CJ
import Data.Either (Either(..))
import JSON.Path as JSON.Path
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

nixManifestCodec :: CJ.Codec NixManifest
nixManifestCodec = Codec.codec' decode encode
  where
  encode = case _ of
    SpagoManifest manifest -> CJ.encode combinedManifestCodec manifest
    PursManifest manifest -> CJ.encode githubBinaryManifestCodec manifest
    PursTidyManifest manifest -> CJ.encode npmRegistryManifestCodec manifest
    PursBackendEsManifest manifest -> CJ.encode npmRegistryManifestCodec manifest
    PursLanguageServerManifest manifest -> CJ.encode npmRegistryManifestCodec manifest
    NamedManifest manifest -> CJ.encode namedManifestCodec manifest

  decode json = except $
    map SpagoManifest (CJ.decode combinedManifestCodec json)
      <|> map PursManifest (CJ.decode githubBinaryManifestCodec json)
      <|> map PursTidyManifest (CJ.decode npmRegistryManifestCodec json)
      <|> map PursBackendEsManifest (CJ.decode npmRegistryManifestCodec json)
      <|> map PursLanguageServerManifest (CJ.decode npmRegistryManifestCodec json)
      <|> map NamedManifest (CJ.decode namedManifestCodec json)
      <|> Left (CJ.DecodeError.DecodeError { path: JSON.Path.Tip, message: "Expected a CombinedManifest, GitHubBinaryManifest, NPMRegistryManifest, or NamedManifest", causes: [] })
