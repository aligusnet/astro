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

import Control.Monad (unless)

import Data.Astro.TypesTest (testDecimalDegrees, testDecimalHours)
import Data.Astro.Time.JulianDate (JulianDate(..))
import Data.Astro.Coordinate
import Data.Astro.Types
import Data.Astro.Utils (reduceToZeroRange)

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
             , testDecimalHours "HA 9.87 in 1980-04-22 14:36:51.67-4 to RA"
                 0.0000001
                 (DH 9.873237)
                 (raToHA (DH 18.539167) (DD (-64.0)) (-4) (JD 2444352.108931366))
             , testProperty "RA <-> HA property for Novosibirsk" $ prop_HARAConv (DD 83) 7 (JD 2457607.97281)
             , testProperty "RA <-> HA property for Rio de Janeiro" $ prop_HARAConv (DD (-43)) (-3) (JD 2457607.97281)
             ]
         , testGroup "EC <-> HC" [
             testHC "EC2 23.219 5.862 -> HC 19.334 283.271"
               0.00000001
               (HC (DD 19.334345224) (DD 283.27102726))
               (equatorialToHorizon (DD 52) (EC2 (DD 23.219444444) (DH 5.862222222222222)))
             ]
        ]

prop_DegreeMSConversion d =
  let dms = toDegreeMS $ DD d
      DD d' = fromDegreeMS dms
  in abs(d-d') < 0.0000001
  where types = (d::Double)

prop_HARAConv longitude timeZone jd dh =
  let dh' = reduceToZeroRange 24 dh
      DH ra = haToRA (raToHA (DH dh') longitude timeZone jd) longitude timeZone jd
      DH ha = raToHA (haToRA (DH dh') longitude timeZone jd) longitude timeZone jd
      eps = 0.00000001
  in abs (ra - dh') < eps && (ha - dh') < eps
  where types = (dh::Double)

testHC msg eps expected actual =
  testCase msg $ assertHC eps expected actual

assertHC eps expected@(HC (DD eAlt) (DD eAz)) actual@(HC (DD aAlt) (DD aAz)) =
  unless (abs(eAlt-aAlt) <= eps && abs(eAz-aAz) <= eps) (assertFailure msg)
  where msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual ++
              "\n (maximum margin of error: " ++ show eps ++ ")"
