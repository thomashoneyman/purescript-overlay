module Test.Lib.Manifest where

import Prelude

import Data.Array as Array
import Data.String as String
import Data.Traversable (for)
import Lib.NixManifest as NixManifest
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
    allPaths <- FS.Aff.readdir manifestFixturesPath
    let fixturePaths = Array.filter (not <<< String.contains (String.Pattern "named")) allPaths
    fixtures <- for fixturePaths \path -> do
      rawManifest <- FS.Aff.readTextFile UTF8 $ Path.concat [ manifestFixturesPath, path ]
      pure { label: path, value: String.trim rawManifest }
    Utils.shouldRoundTrip "Manifest" NixManifest.nixManifestCodec fixtures
