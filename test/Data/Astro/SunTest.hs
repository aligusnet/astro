module Data.Astro.SunTest
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

import Data.Astro.TypesTest (testDecimalDegrees)
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
             -- The Astronomycal Almanach gives (EC1 (fromDMS 19 21 16) (fromHMS 8 23 33))
             testEC1 "(1) at 2003-07-27 00:00:00"
                 0.000001
                 (EC1 (fromDMS 19 21 17.4394) (fromHMS 8 23 32.8286))
                 (sunPosition1 j2010SunDetails (JD 2452847.5))
             , testEC1 "(1) at 2016-08-11 13:30:00"
                 0.000001
                 (EC1 (fromDMS 15 2 5.1480) (fromHMS 9 26 49.9629))
                 (sunPosition1 j2010SunDetails (JD 2457612.0625))
             -- The Astronomycal Almanach gives (EC1 (fromDMS 19 12 52) (fromHMS 8 26 3))
             , testEC1 "(1) at 1988-07-27 00:00:00"
                 0.000001
                 (EC1 (fromDMS 19 12 50.6369) (fromHMS 8 26 3.6147))
                 (sunPosition1 j2010SunDetails (JD 2447369.5))
             -- The Astronomycal Almanach gives (EC1 (fromDMS 19 12 52) (fromHMS 8 26 3))
             , testEC1 "(2) at 1988-07-27 00:00:00"
                 0.000001
                 (EC1 (fromDMS 19 12 48.9604) (fromHMS 8 26 4.1004))
                 (sunPosition2 (JD 2447369.5))
             -- The Astronomycal Almanach gives (EC1 (fromDMS 19 21 16) (fromHMS 8 23 33))
             , testEC1 "(2) at 2003-07-27 00:00:00"
                 0.000001
                 (EC1 (fromDMS 19 21 9.2633) (fromHMS 8 23 35.2376))
                 (sunPosition2 (JD 2452847.5))
             ]
           , testGroup "distance" [
               testCase "at 1988-07-27 00:00:00" $ assertApproxEqual ""
                   1
                   151920130
                   (sunDistance (JD 2447369.5))
               , testCase "at 2016-08-12 13:30:00" $ assertApproxEqual ""
                   1
                   151577454
                   (sunDistance (JD 2457613.0625))
               , testCase "at 2010-10-10 18:00:00" $ assertApproxEqual ""
                   1
                   149373939
                   (sunDistance (JD 2455480.25))
               ]
           , testGroup "angular size" [
               testDecimalDegrees "at 1988-07-27 00:00:00"
                   0.000001
                   (fromDMS 0 31 29.9308)
                   (sunAngularSize (JD 2447369.5))
               , testDecimalDegrees "at 2016-08-12 13:30:00"
                   0.000001
                   (fromDMS 0 31 34.2034)
                   (sunAngularSize (JD 2457613.0625))
               , testDecimalDegrees "at 2010-10-10 18:00:00"
                   0.000001
                   (fromDMS 0 32 2.1461)
                   (sunAngularSize (JD 2455480.25))
               ]
        ]

testSunDetails msg eps expected actual =
  testCase msg $ assertSunDetails eps expected actual

assertSunDetails eps expected@(SunDetails (JD eJd) (DD eEps) (DD eOm) eE) actual@(SunDetails (JD aJd) (DD aEps) (DD aOm) aE) =
  unless (abs(eJd-aJd) <= eps && abs(eEps-aEps) <= eps
          && abs(eOm-aOm) <= eps && abs(eE-aE) <= eps) (assertFailure msg)
  where msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual ++
              "\n (maximum margin of error: " ++ show eps ++ ")"