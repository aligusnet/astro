module Data.Astro.UtilsTest
(
  tests
)

where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.HUnit.Approx
import Test.QuickCheck

import Data.Fixed(Pico(..))

import Data.Astro.Utils

tests = [testGroup "fromFixed" [
            testProperty "fromFixed Property" prop_fromFixed
            ]
        , testGroup "fraction" [
            testProperty "fraction Property" prop_fraction
            ]
        , testGroup "trunc" [
            testProperty "trunc Property" prop_trunc
            , testCase "5.562" $ 5.0 @=? trunc 5.562
            , testCase "-7.93" $ (-7.0) @=? trunc (-7.93)
            ]
        ]

prop_fromFixed d =
  abs((fromFixed ((realToFrac d)::Pico))-d) < 0.0000001
  where types = (d::Double)

prop_fraction d =
  let (i, f) = fraction d
      f' = d - fromIntegral i
  in i == truncate d && abs(f'-f) < 0.0000001
  where types = (d::Double)

prop_trunc d =
  let d' = fromIntegral $ truncate d
  in abs(d' - trunc d) < 0.0000001
  where types = (d::Double)
