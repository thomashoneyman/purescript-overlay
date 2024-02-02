module Example.Simple.Tested.Tests.Decimals where

import Prelude

import Example.Simple.Tested (roundTrip)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec :: Spec Unit
spec =
  describe "decimals" do
    describe "roundtrip" do
      it "is identity"
        $ quickCheck \n -> roundTrip n == n
