module Test.Lib where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff as Aff
import Lib.Nix.Prefetch as Nix
import Node.Path as Path
import Registry.Sha256 as Sha256
import Test.Lib.Manifest as Manifest
import Test.Spec as Spec
import Test.Spec.Assertions as Assert
import Test.Spec.Reporter as Spec.Reporter
import Test.Spec.Runner as Spec.Runner

specReporter :: Array Spec.Runner.Reporter
specReporter = [ Spec.Reporter.consoleReporter ]

specConfig :: Spec.Runner.Config
specConfig = Spec.Runner.defaultConfig { timeout = Just $ Milliseconds 10_000.0 }

main :: Effect Unit
main = Aff.launchAff_ $ Spec.Runner.runSpec' specConfig specReporter do
  Spec.describe "Manifest"
    Manifest.spec

  Spec.describe "Nix" do
    Spec.it "prefetch-npm-deps" do
      let path = Path.concat [ "..", "manifests", "build-support", "spago", "0.93.x.json" ]
      Nix.prefetchNpmDeps path >>= case _ of
        Left err -> Assert.fail err
        Right value -> Assert.shouldEqual (Sha256.print value) "sha256-jcvoWDsCvQ7M5G3kf6UMXJQ23/PZUwRhPEgioRhvqa8="

    Spec.it "nix prefetch + hash" do
      let url = "https://raw.githubusercontent.com/thomashoneyman/purescript-overlay/e19bca38baed52d71fb245c0d78bc5bd74e59800/overlay.nix"
      Nix.nixPrefetchUrl url >>= case _ of
        Left err -> Assert.fail err
        Right value -> Assert.shouldEqual (Sha256.print value) "sha256-SxrOhzastnt6+dbDIshXzqi2HRuZBdYRQ77bmxHloXk="
