module Data.Astro.SunTest
(
  tests
)

where


import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

import Control.Monad (unless)

import Data.Astro.CoordinateTest (testEC1)

import Data.Astro.Types (DecimalDegrees(..), fromDMS, fromHMS)
import Data.Astro.Time.JulianDate (JulianDate(..))
import Data.Astro.Coordinate (EquatorialCoordinates1(..))
import Data.Astro.Sun

tests = [testGroup "sunDetails" [
            testSunDetails "J2010.0"
              0.000001
              (SunDetails j2010 (DD 279.557208) (DD 283.112438) 0.016705)
              (sunDetails j2010)
            ]
         , testGroup "the Sun's coordinates" [
             testEC1 "(1) at 2003-07-27 00:00:00"
                 0.000001
                 (EC1 (fromDMS 19 21 10.3814) (fromHMS 8 23 33.6581))
                 (sunPosition1 j2010SunDetails (JD 2452847.5))
             , testEC1 "(1) at 2016-08-11 13:30:00"
                 0.000001
                 (EC1 (fromDMS 15 2 9.4605) (fromHMS 9 26 50.3172))
                 (sunPosition1 j2010SunDetails (JD 2457612.0625))
             , testEC1 "(2) at 1988-07-27 00:00:00"
                 0.000001
                 (EC1 (fromDMS 19 12 43.1836) (fromHMS 8 26 3.6132))
                 (sunPosition2 (JD 2447369.5))
             , testEC1 "(2) at 2003-07-27 00:00:00"
                 0.000001
                 (EC1 (fromDMS 19 21 2.2051) (fromHMS 8 23 36.0670))
                 (sunPosition2 (JD 2452847.5))
             ]
        ]

testSunDetails msg eps expected actual =
  testCase msg $ assertSunDetails eps expected actual

assertSunDetails eps expected@(SunDetails (JD eJd) (DD eEps) (DD eOm) eE) actual@(SunDetails (JD aJd) (DD aEps) (DD aOm) aE) =
  unless (abs(eJd-aJd) <= eps && abs(eEps-aEps) <= eps
          && abs(eOm-aOm) <= eps && abs(eE-aE) <= eps) (assertFailure msg)
  where msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual ++
              "\n (maximum margin of error: " ++ show eps ++ ")"
