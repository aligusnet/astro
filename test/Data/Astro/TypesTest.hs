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
        , testGroup "DecimalDegrees <-> Radians" [
            testCase "0 -> 0 (rad)" $ assertApproxEqual "" 0.000000001 0 $ toRadians (DD 0)
            , testCase "45 -> PI/4" $ assertApproxEqual "" 0.000000001 (pi*0.25) $ toRadians (DD 45)
            , testCase "90 -> PI/2" $ assertApproxEqual "" 0.000000001 (pi*0.5) $ toRadians (DD 90)
            , testCase "180 -> PI" $ assertApproxEqual "" 0.000000001 pi $ toRadians (DD 180)
            , testCase "360 -> 2*PI" $ assertApproxEqual "" 0.000000001 (pi*2) $ toRadians (DD 360)
            , testDecimalDegrees "0 -> 0 (deg)" 0.000000001 (DD 0) (fromRadians 0)
            , testDecimalDegrees "pi/4 -> 45" 0.000000001 (DD 45) (fromRadians (pi*0.25))
            , testDecimalDegrees "pi/2 -> 90" 0.000000001 (DD 90) (fromRadians (pi*0.5))
            , testDecimalDegrees "pi -> 180" 0.000000001 (DD 180) (fromRadians pi)
            , testDecimalDegrees "2*pi -> 360" 0.000000001 (DD 360) (fromRadians (pi*2))
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
