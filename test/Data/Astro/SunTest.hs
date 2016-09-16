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


import Data.Astro.Types (DecimalDegrees(..), DecimalHours(..), GeographicCoordinates(..), fromDMS, fromHMS)
import Data.Astro.Time.JulianDate (JulianDate(..), LocalCivilTime(..), lctFromYMDHMS, LocalCivilDate(..), lcdFromYMD)
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
               -- sunrise/set times are from http://timeanddate.com
               -- coordinates are from http://dateandtime.info
               testRiseSet "Venice at 2016-08-12; Rise: 06:08, DD 68; Set: 20:22, DD 292"
                   0.000001
                   (RiseSet
                      (Just (lctFromYMDHMS 2 2016 8 12 6 8 1.2955, DD 67.6609383))
                      (Just (lctFromYMDHMS 2 2016 8 12 20 22 17.5593, DD 292.0714064)))
                   (sunRiseAndSet (GeoC (DD 45.43713) (12.33265)) 0.833333 (lcdFromYMD 2 2016 8 12))
               , testRiseSet "Ulaanbaatar at 2016-08-13; Rise: 06:45, DD 67; Set: 21:09, DD 293"
                   0.000001
                   (RiseSet
                      (Just (lctFromYMDHMS 9 2016 8 13 6 44 43.1358, DD 66.8644763))
                      (Just (lctFromYMDHMS 9 2016 8 13 21 8 49.7896, DD 292.8475000)))
                   (sunRiseAndSet (GeoC (DD 47.90771) (106.88324)) 0.833333 (lcdFromYMD 9 2016 8 13))
               , testRiseSet "Lima at 2016-08-12; Rise: 06:22, DD 75; Set: 18:04, DD 285"
                   0.000001
                   (RiseSet
                      (Just (lctFromYMDHMS (-5) 2016 8 12 6 22 22.2308, DD 75.0822853))
                      (Just (lctFromYMDHMS (-5) 2016 8 12 18 3 41.7631, DD 284.7661756)))
                   (sunRiseAndSet (GeoC (DD $ -12.04318) (DD $ -77.02824)) 0.833333 (lcdFromYMD (-5) 2016 8 12))
               , testRiseSet "Longyearbyen at 2016-08-12; Circumpolar"
                   0.000001
                   Circumpolar
                   (sunRiseAndSet (GeoC (DD 78.22) (DD 15.65)) 0.833333 (lcdFromYMD 2 2016 8 12))
               , testRiseSet "Longyearbyen at 2017-01-12; Down all day"
                   0.000001
                   NeverRises
                   (sunRiseAndSet (GeoC (DD 78.22) (DD 15.65)) 0.833333 (lcdFromYMD 2 2017 1 12))
               , testRiseSet "Anchorage at 2016-08-13; Rise: 06:05, DD 57; Set: 22:02, DD 302"
                   0.000001
                   (RiseSet
                      (Just (lctFromYMDHMS (-8) 2016 8 13 6 4 29.3945, DD 57.0595036))
                      (Just (lctFromYMDHMS (-8) 2016 8 13 22 2 8.3067, DD 302.4495529)))
                   (sunRiseAndSet (GeoC (DD 61.21806) (-149.90028)) 0.833333 (lcdFromYMD (-8) 2016 8 13))
               , testRiseSet "Kazan at 2016-08-18; Rise: 04:21, DD 65; Set: 19:12, DD 295"
                   0.000001
                   (RiseSet
                      (Just (lctFromYMDHMS 3 2016 8 18 4 21 19.3153, DD 65.0507218))
                      (Just (lctFromYMDHMS 3 2016 8 18 19 11 46.7175,DD 294.5658995)))
                   (sunRiseAndSet (GeoC 55.78874 49.1221400) 0.833333 $ lcdFromYMD 3 2016 8 18)
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

eqRS eps (LCT (DH tz1) (JD j1), DD d1) (LCT (DH tz2) (JD j2), DD d2) = abs (j1-j2) < eps && abs (tz1-tz2) < eps && abs (d1-d2) < eps
