module Data.Astro.Time.SiderealTest
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

import Data.Astro.Time.JulianDateTest (testJD)

import Data.Astro.Coordinate (DecimalDegrees(..))
import Data.Astro.Time.Types (TimeOfDay(..))
import qualified Data.Astro.Time.Time as T
import Data.Astro.Time.JulianDate (JulianDate(..), splitToDayAndTime)
import Data.Astro.Time.Sidereal

tests = [testGroup "GST <-> UT conversions" [
            testJD "1980-04-22 14:36:51.67 UT -> 1980-04-22 04:40:05.23 GST"
                0.0000001
                (JD 2444351.694504972)
                (utToGST $ JD 2444352.108931366)
            , testJD "2016-08-04 19:28:43.15 UT -> 2016-08-04 16:23:52.84 GST"
                0.0000001
                (JD 2457605.183251656)
                (utToGST $ JD 2457605.3116105325)
            , testJD "1980-04-22 04:40:05.23 GST -> 1980-04-22 14:36:51.67 UT"
                0.0000001
                (JD 2444352.108931366)
                (gstToUT $ JD 2444351.694504972)
            , testJD "2016-08-04 16:23:52.84 GST -> 2016-08-04 19:28:43.15 UT"
                0.0000001
                (JD 2457605.3116105325)
                (gstToUT $ JD 2457605.183251656)
            , testProperty "property" prop_siderealTimeConversions
            ]
         , testGroup "GST <-> LST converions" [
             testJD "2016-08-05 04:40:05.23 GST -> 2016-08-05 00:24:05.23 LST"
               0.00000001
               (JD 2457605.516727199)
               (gstToLST (DD $ -64) $ JD 2457605.6945049767)
           , testJD "1980-04-22 15:25:35.12 GST -> 1980-04-22 20:20:27.91 LST"
               0.00000001
               (JD 2444352.34754537)
               (gstToLST (DD 73.72) $ JD 2444352.1427675923)
           , testJD "1985-07-01 21:41:25.78 GST -> 1985-07-02 04:36:13.78 LST"
               0.00000001
               (JD 2446248.6918261573)
               (gstToLST (DD 103.7) $ JD 2446248.403770602)
           , testJD "2016-08-05 00:24:05.23 LST -> 2016-08-05 04:40:05.23 GST"
               0.00000001
               (JD 2457605.6945049767)
               (lstToGST (DD $ -64) $ JD 2457605.516727199)
           , testJD "1980-04-22 20:20:27.91 LST -> 1980-04-22 15:25:35.12 GST"
               0.00000001
               (JD 2444352.1427675923)
               (lstToGST (DD 73.72) $ JD 2444352.34754537)
             ]
           , testJD "1985-07-02 04:36:13.78 LST -> 1985-07-01 21:41:25.78 GST"
               0.00000001
               (JD 2446248.403770602)
               (lstToGST (DD 103.7) $ JD 2446248.6918261573)
           , testProperty "property longitude=-101.13" $ prop_localGlobalConverions (DD $ -101.13)
           , testProperty "property longitude=31.7" $ prop_localGlobalConverions (DD 31.7)
        ]

prop_siderealTimeConversions =
  forAll (choose (0, 999999999)) $ check
  where check utN =
          let utJd = JD utN
              sdJd = utToGST utJd
              utJd'@(JD utN') = gstToUT sdJd
              (JD utD, _) = splitToDayAndTime utJd
              (JD sdD, _) = splitToDayAndTime sdJd
              (_, JD utT') = splitToDayAndTime utJd'
              hasAmbigity = utT' < T.toDecimalHours (TimeOfDay 0 3 57)
              eps = 0.0000001
          in (hasAmbigity || abs(utN-utN') < eps) && (truncate utD) == (truncate sdD)

prop_localGlobalConverions longitude =
  forAll (choose (1, 999999999)) $ check
  where check n =
          let jd = JD n
              JD a1 = lstToGST longitude $ gstToLST longitude jd
              JD a2 = gstToLST longitude $ lstToGST longitude jd
              eps = 0.000000001
          in abs(n-a1) < eps && abs(n-a2) < eps
