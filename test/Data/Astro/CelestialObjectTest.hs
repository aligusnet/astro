module Data.Astro.CelestialObjectTest
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

import Data.Astro.TypesTest (testDecimalDegrees)

import Control.Monad (unless)

import Data.Astro.Types (DecimalDegrees(..), DecimalHours(..), fromDMS, fromHMS)
import Data.Astro.Coordinate (EquatorialCoordinates1(..), EclipticCoordinates(..))
import Data.Astro.CelestialObject

tests = [testGroup "angle" [
            testDecimalDegrees "Equatorial: Orionis and Canis Majoris"
                0.000000001
                (DD 23.673849422164192)
                (angleEquatorial (EC1 (DD (-8.225)) (DH 5.225472222222222)) (EC1 (DD (-16.68638888888889)) (DH 6.737055555555555)))
            , testDecimalDegrees "Ecliptic: Orionis and Canis Majoris"
                0.000000001
                (DD 23.673849422164192)
                (angleEcliptic (EcC (DD (-31.12290508933333)) (DD 76.53651836408739)) (EcC (DD (-39.597832824969665)) (DD 103.79168740150627)))
            ]
         , testGroup "riseAndSet" [
             testRiseAndSet "first"
             0.000001
             (RiseSet (DH 16.721731, DD 64.362370) (DH 6.589380, DD 295.637630))
             (riseAndSet (EC1 (fromDMS 21 42 0) (fromHMS 23 39 20)) (fromDMS 0 34 0) (DD 30))
             ]
        ]

testRiseAndSet msg eps expected actual =
  testCase msg $ assertRiseAndSet eps expected actual

assertRiseAndSet eps expected@(RiseSet (DH etr, DD ear) (DH ets, DD eas)) actual@(RiseSet (DH atr, DD aar) (DH ats, DD aas)) =
  unless (abs(etr-atr) <= eps && abs(ear-aar) <= eps
          && abs(ets-ats) <= eps && abs(eas-aas) <= eps) (assertFailure msg)
  where msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual ++
              "\n (maximum margin of error: " ++ show eps ++ ")"
