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

import Data.Astro.TypesTest (testDecimalDegrees, testDecimalHours)
import Data.Astro.CoordinateTest (testEC1)

import Data.Astro.Types (DecimalDegrees(..), GeographicCoordinates(..), fromDMS, fromHMS)
import Data.Astro.Time.JulianDate (JulianDate(..))
import Data.Astro.Time.Epoch (j2010)
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
           , testGroup "Sun's rise and set" [
               -- timeanddate.com: Rise: 06:08, DD 68; Set: 20:22, DD 292
               testRiseSet "Venice at 2016-08-12"
                   0.000001
                   (RiseSet (Just (JD 2457612.7555686, DD 67.6606896)) (Just (JD 2457613.3488124, DD 292.0719405)))
                   (sunRiseAndSet (GeoC (DD 45.43713) (12.33265)) 2 0.833333 (JD 2457612.5))
               -- timeanddate.com: Rise: 06:45, DD 67; Set: 21:09, DD 293
               , testRiseSet "Ulaanbaatar at 2016-08-13"
                   0.000001
                   (RiseSet (Just (JD 2457613.7810581, DD 66.8649010)) (Just (JD 2457614.3811317, DD 292.8475452)))
                   (sunRiseAndSet (GeoC (DD 47.90771) (106.88324)) 9 0.833333 (JD 2457613.5))
               -- timeanddate.com: Rise: 06:22, DD 75; Set: 18:04, DD 285
               , testRiseSet "Lima at 2016-08-12"
                   0.000001
                   (RiseSet (Just (JD 2457612.7655317, DD 75.0818488)) (Just (JD 2457613.2525567, DD 284.7672950)))
                   (sunRiseAndSet (GeoC (DD $ -12.04318) (DD $ -77.02824)) (-5) 0.833333 (JD 2457612.5))
               -- timeanddate.com: Circumpolar
               , testRiseSet "Longyearbyen at 2016-08-12"
                   0.000001
                   Circumpolar
                   (sunRiseAndSet (GeoC (DD 78.22) (DD 15.65)) 2 0.833333 (JD 2457612.5))
               -- timeanddate.com: Down all day
               , testRiseSet "Longyearbyen at 2017-01-12"
                   0.000001
                   NeverRises
                   (sunRiseAndSet (GeoC (DD 78.22) (DD 15.65)) 2 0.833333 (JD 2457765.5))
               -- timeanddate.com: Rise: 06:05, DD 57; Set: 22:02, DD 302
               , testRiseSet "Anchorage at 2016-08-13"
                   0.000001
                   (RiseSet (Just (JD 2457613.7531018, DD 57.0568583)) (Just (JD 2457614.4181508, DD 302.4505098)))
                   (sunRiseAndSet (GeoC (DD 61.21806) (-149.90028)) (-8) 0.833333 (JD 2457613.5))
               ],
             testGroup "equationOfTime" [
               testDecimalHours "zero at June"
                   (1/3600)
                   (-(fromHMS 0 0 1))
                   (equationOfTime $ JD 2457551.5)
               , testDecimalHours "minimum at February"
                   (1/3600)
                   (-(fromHMS 0 14 7))
                   (equationOfTime $ JD 2457429.5)
               , testDecimalHours "maximum at November"
                   (1/3600)
                   (fromHMS 0 16 18)
                   (equationOfTime $ JD 2457694.5)
               ]
             , testGroup "solarElongation" [
                 testDecimalDegrees "Mars at 2010-07-27 20:00:00 UT"
                     0.0000001
                     (DD 24.7905087)
                     (solarElongation (EC1 (fromDMS 11 57 27) (fromHMS 10 6 45)) (JD 2455405.3333333335))
                 ]
        ]

testSunDetails msg eps expected actual =
  testCase msg $ assertSunDetails eps expected actual

assertSunDetails eps expected@(SunDetails (JD eJd) (DD eEps) (DD eOm) eE) actual@(SunDetails (JD aJd) (DD aEps) (DD aOm) aE) =
  unless (abs(eJd-aJd) <= eps && abs(eEps-aEps) <= eps
          && abs(eOm-aOm) <= eps && abs(eE-aE) <= eps) (assertFailure msg)
  where msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual ++
              "\n (maximum margin of error: " ++ show eps ++ ")"

testRiseSet msg eps expected actual =
  testCase msg $ assertRiseSet eps expected actual

assertRiseSet eps expected@(RiseSet er es) actual@(RiseSet ar as) =
  unless (eqMaybeRS eps er ar && eqMaybeRS eps es as) (assertFailure msg)
  where msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual ++
              "\n (maximum margin of error: " ++ show eps ++ ")"
assertRiseSet _ Circumpolar Circumpolar = assertString ""
assertRiseSet _ Circumpolar actual = assertString msg
  where msg = "expected: Circumpolar\n but got: " ++ show actual
assertRiseSet _ NeverRises NeverRises = assertString ""
assertRiseSet _ NeverRises actual = assertString msg
  where msg = "expected: NeverRises\n but got: " ++ show actual


eqMaybeRS eps (Just rs1) (Just rs2) = eqRS eps rs1 rs2
eqMaybeRS _ Nothing Nothing = True
eqMaybeRS _ _ _ = False

eqRS eps (JD j1, DD d1) (JD j2, DD d2) = abs (j1-j2) < eps && abs (d1-d2) < eps
