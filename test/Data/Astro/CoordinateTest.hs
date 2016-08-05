module Data.Astro.CoordinateTest
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

import Data.Astro.Coordinate

tests = [testGroup "DecimalDegrees to DegreeMS" [
            testDecimalDegrees "182 31' 27''" 0.00001 (DD 182.52417) $ toDecimalDegrees (DegreeMS 182 31 27)
            , testCase "182.5" $ fromDecimalDegrees (DD 182.5) @?= DegreeMS 182 30 0
            , testProperty "DecimalDegrees Comversions Property" prop_DecimalDegreesConversion
            ]
        ]


testDecimalDegrees msg eps (DD expected) (DD actual) =
  testCase msg $ assertApproxEqual "" eps expected actual


prop_DecimalDegreesConversion d =
  let dms = fromDecimalDegrees $ DD d
      DD d' = toDecimalDegrees dms
  in abs(d-d') < 0.0000001
  where types = (d::Double)
