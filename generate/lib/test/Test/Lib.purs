module Test.Lib where

import Prelude

import Effect (Effect)
import Test.Lib.Manifest as Manifest
import Test.Spec as Spec
import Test.Spec.Reporter as Spec.Reporter
import Test.Spec.Runner.Node as Spec.Runner.Node

main :: Effect Unit
main = Spec.Runner.Node.runSpecAndExitProcess [ Spec.Reporter.consoleReporter ] do
  Spec.describe "Manifest"
    Manifest.spec
