module Data.Astro.Time.SiderealTest
(
  tests
  , testLST
)

where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

import Control.Monad (unless)

import Data.Astro.TypesTest (testDecimalHours)
import Data.Astro.Time.JulianDateTest (testJD)

import Data.Astro.Utils (reduceToZeroRange)
import Data.Astro.Types (DecimalHours(..), fromHMS)
import Data.Astro.Coordinate (DecimalDegrees(..))
import Data.Astro.Time.JulianDate (JulianDate(..), splitToDayAndTime)
import Data.Astro.Time.Sidereal


eps = 1/(24*60*60*10)

tests = [testGroup "GST <-> UT conversions" [
            testGST "1980-04-22 14:36:51.67 UT -> 1980-04-22 04:40:05.23 GST"
                eps
                (hmsToGST 4 40 5.23)
                (utToGST $ JD 2444352.108931366)
            , testGST "2016-08-04 19:28:43.15 UT -> 2016-08-04 16:23:52.94 GST"
                eps
                (hmsToGST 16 23 52.94)
                (utToGST $ JD 2457605.3116105325)
            , testJD "1980-04-22 04:40:05.23 GST -> 1980-04-22 14:36:51.67 UT"
                eps
                (JD 2444352.108931366)
                (gstToUT (JD 2444351.5) (hmsToGST 4 40 05.23))
            , testJD "2016-08-04 16:23:52.94 GST -> 2016-08-04 19:28:43.15 UT"
                eps
                (JD 2457605.3116105325)
                (gstToUT (JD 2457604.5) (hmsToGST 16 23 52.94))
            , testProperty "property" prop_siderealTimeConversions
            ]
         , testGroup "GST <-> LST converions" [
             testLST "04:40:05.23 GST -> 00:24:05.23 LST"
               eps
               (hmsToLST 0 24 5.23)
               (gstToLST (DD $ -64) (hmsToGST 4 40 5.23))
           , testLST "15:25:35.12 GST -> 20:20:27.92 LST"
               eps
               (hmsToLST 20 20 27.92)
               (gstToLST (DD 73.72) (hmsToGST 15 25 35.12))
           , testLST "21:41:25.78 GST -> 04:36:13.78 LST"
               eps
               (hmsToLST 4 36 13.78)
               (gstToLST (DD 103.7) (hmsToGST 21 41 25.78))
           , testGST "00:24:05.23 LST -> 04:40:05.23 GST"
               eps
               (hmsToGST 4 40 5.23)
               (lstToGST (DD $ -64) (hmsToLST 0 24 5.23))
           , testGST "20:20:27.92 LST -> 15:25:35.12 GST"
               eps
               (hmsToGST 15 25 35.12)
               (lstToGST (DD 73.72) (hmsToLST 20 20 27.92))
             ]
           , testGST "04:36:13.78 LST -> 21:41:25.78 GST (w/ DC)"
               eps
               (hmsToGST (-2) (-18) (-34.22))
               (lstToGSTwDC (DD 103.7) (hmsToLST 4 36 13.78))
           , testGST "04:36:13.78 LST -> 21:41:25.78 GST"
               eps
               (hmsToGST 21 41 25.78)
               (lstToGST (DD 103.7) (hmsToLST 4 36 13.78))
           , testProperty "property longitude=-101.13" $ prop_localGlobalConverions (DD $ -101.13)
           , testProperty "property longitude=31.7" $ prop_localGlobalConverions (DD 31.7)
        ]

testLST msg eps expected actual = testDecimalHours msg eps (lstToDH expected) (lstToDH actual)
testGST msg eps expected actual = testDecimalHours msg eps (gstToDH expected) (gstToDH actual)

prop_siderealTimeConversions =
  forAll (choose (0, 999999999)) $ check
  where check utN =
          let utJd = JD utN
              gst = utToGST utJd
              utJd'@(JD utN') = gstToUT utJd gst
              (JD utD, _) = splitToDayAndTime utJd
              (_, JD utT') = splitToDayAndTime utJd'
              DH diff = fromHMS 0 3 57
              hasAmbigity = utT' < diff
              eps = 0.0000001
          in (hasAmbigity || abs(utN-utN') < eps)


prop_localGlobalConverions longitude =
  forAll (choose (0, 24)) $ check
  where check n =
          let dh = (DH n)
              gst = lstToGST24 longitude $ gstToLST longitude $ dhToGST dh
              lst = gstToLST longitude $ lstToGST24 longitude $ dhToLST dh
              eps = 0.000000001
              DH a1 = gstToDH gst
              DH a2 = lstToDH lst
          in abs(n-a1) < eps && abs(n-a2) < eps
        lstToGST24 longitude lst =
          let gst = lstToGST longitude lst
              dh24 = reduceToZeroRange 24 $ gstToDH gst
          in dhToGST dh24
