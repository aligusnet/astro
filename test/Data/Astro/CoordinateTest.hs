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

tests = [testGroup "DecimalDegrees <-> DegreeMS" [
            testDecimalDegrees "182 31' 27''" 0.00001 (DD 182.52417) $ fromDegreeMS (DegreeMS 182 31 27)
            , testCase "182.5" $ toDegreeMS (DD 182.5) @?= DegreeMS 182 30 0
            , testProperty "property" prop_DegreeMSConversion
            ]
        , testGroup "DecimalDegrees <-> DecimalHours" [
            testDecimalDegrees "12.03 H -> 180.45 D" 0.000001 (DD 180.45) $ fromDecimalHours (DH 12.03)
            , testDecimalHours "180.45 D -> 12.03 H" 0.000001 (DH 12.03) $ toDecimalHours (DD 180.45)
            , testProperty "property" prop_DHConversion
            ]
        ]


testDecimalDegrees msg eps (DD expected) (DD actual) =
  testCase msg $ assertApproxEqual "" eps expected actual

testDecimalHours msg eps (DH expected) (DH actual) =
  testCase msg $ assertApproxEqual "" eps expected actual

prop_DegreeMSConversion d =
  let dms = toDegreeMS $ DD d
      DD d' = fromDegreeMS dms
  in abs(d-d') < 0.0000001
  where types = (d::Double)

prop_DHConversion n =
  let DH h = toDecimalHours . fromDecimalHours $ DH n
      DD d = fromDecimalHours . toDecimalHours $ DD n
      eps = 0.00000001
  in abs(n-h) < eps && abs(n-d) < eps
  where types = (n::Double)      
