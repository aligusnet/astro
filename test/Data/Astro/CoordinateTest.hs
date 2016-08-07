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

import Data.Astro.TypesTest (testDecimalDegrees, testDecimalHours)
import Data.Astro.Time.JulianDate (JulianDate(..))
import Data.Astro.Coordinate
import Data.Astro.Types

tests = [testGroup "DecimalDegrees <-> DegreeMS" [
            testDecimalDegrees "182 31' 27''" 0.00001 (DD 182.52417) $ fromDegreeMS (DegreeMS 182 31 27)
            , testCase "182.5" $ toDegreeMS (DD 182.5) @?= DegreeMS 182 30 0
            , testProperty "property" prop_DegreeMSConversion
            ]
         , testGroup "RA <-> HA" [
             testDecimalHours "RA 18.53 in 1980-04-22 14:36:51.67-4 to HA"
               0.0000001
               (DH 9.873237)
               (raToHA (DH 18.539167) (DD (-64.0)) (-4) (JD 2444352.108931366))
                                 ]
        ]

prop_DegreeMSConversion d =
  let dms = toDegreeMS $ DD d
      DD d' = fromDegreeMS dms
  in abs(d-d') < 0.0000001
  where types = (d::Double)
