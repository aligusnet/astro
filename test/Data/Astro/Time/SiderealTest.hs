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

import Data.Astro.Time (TimeOfDay(..), toDecimalHours)
import Data.Astro.Time.JulianDate (JulianDate(..), splitToDayAndTime)
import Data.Astro.Time.Sidereal

tests = [testGroup "GST <-> UT conversions" [
            testJD "1980-04-22 14:36:51.67 UT"
                0.0000001
                (JD 2444351.694504972)
                (utToGST $ JD 2444352.108931366)
            , testJD "2016-08-04 19:28:43.15 UT"
                0.0000001
                (JD 2457605.183251656)
                (utToGST $ JD 2457605.3116105325)
            , testJD "1980-04-22 04:40:05.23 GST"
                0.0000001
                (JD 2444352.108931366)
                (gstToUT $ JD 2444351.694504972)
            , testJD "2016-08-04 16:23:52.84 GST"
                0.0000001
                (JD 2457605.3116105325)
                (gstToUT $ JD 2457605.183251656)
            , testProperty "property" prop_siderealTimeConversions
            ]
         , testGroup "GST <-> LST converions" [
             testJD "2016-08-05 04:40:05.23 GST"
               0.00000001
               (JD 2457605.516727199)
               (gstToLST (-64) $ JD 2457605.6945049767)
             , testJD "1980-04-22 15:25:35.12 GST"
               0.00000001
               (JD 2444352.34754537)
               (gstToLST 73.72 $ JD 2444352.1427675923)
             ]
        ]

testJD msg eps expected actual =
  testCase msg $ assertJD eps expected actual

assertJD eps (JD expected) (JD actual) =
  unless (abs(expected-actual) <= eps) (assertFailure msg)
  where msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual ++
              "\n (maximum margin of error: " ++ show eps ++ ")"

prop_siderealTimeConversions =
  forAll (choose (0, 999999999)) $ check
  where check utN =
          let utJd = JD utN
              sdJd = utToGST utJd
              utJd'@(JD utN') = gstToUT sdJd
              (JD utD, _) = splitToDayAndTime utJd
              (JD sdD, _) = splitToDayAndTime sdJd
              (_, JD utT') = splitToDayAndTime utJd'
              hasAmbigity = utT' < toDecimalHours (TimeOfDay 0 3 57)
              eps = 0.0000001
          in (hasAmbigity || abs(utN-utN') < eps) && (truncate utD) == (truncate sdD)
