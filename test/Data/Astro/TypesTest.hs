module Data.Astro.TypesTest
(
  tests
  , testDecimalDegrees
  , testDecimalHours
)

where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.HUnit.Approx
import Test.QuickCheck

import Data.Astro.Types

tests = [testGroup "DecimalDegrees <-> DecimalHours" [
            testDecimalDegrees "12.03 H -> 180.45 D" 0.000001 (DD 180.45) $ fromDecimalHours (DH 12.03)
            , testDecimalHours "180.45 D -> 12.03 H" 0.000001 (DH 12.03) $ toDecimalHours (DD 180.45)
            , testProperty "property" prop_DHConversion
            ]
        ]


testDecimalDegrees msg eps (DD expected) (DD actual) =
  testCase msg $ assertApproxEqual "" eps expected actual

testDecimalHours msg eps (DH expected) (DH actual) =
  testCase msg $ assertApproxEqual "" eps expected actual

prop_DHConversion n =
  let DH h = toDecimalHours . fromDecimalHours $ DH n
      DD d = fromDecimalHours . toDecimalHours $ DD n
      eps = 0.00000001
  in abs(n-h) < eps && abs(n-d) < eps
  where types = (n::Double)      
