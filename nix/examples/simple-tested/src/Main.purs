module Example.Simple.Tested where

import Prelude

import Data.Decimal (fromNumber, toNumber)

roundTrip :: Number -> Number
roundTrip = toNumber <<< fromNumber
