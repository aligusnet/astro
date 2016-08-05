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

tests = [testGroup "siderealTime" [
            testJD "1980-04-22 14:36:51.67 UT"
                0.0000001
                (JD 2444351.694504972)
                (toSiderealTime $ JD 2444352.108931366)
            , testJD "2016-08-04 19:28:43.15 UT"
                0.0000001
                (JD 2457605.183251656)
                (toSiderealTime $ JD 2457605.3116105325)
            , testJD "1980-04-22 04:40:05.23 GST"
                0.0000001
                (JD 2444352.108931366)
                (fromSiderealTime $ JD 2444351.694504972)
            , testJD "2016-08-04 16:23:52.84 GST"
                0.0000001
                (JD 2457605.3116105325)
                (fromSiderealTime $ JD 2457605.183251656)
            , testProperty "property" prop_siderealTimeConversions
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
              sdJd = toSiderealTime utJd
              utJd'@(JD utN') = fromSiderealTime sdJd
              (JD utD, _) = splitToDayAndTime utJd
              (JD sdD, _) = splitToDayAndTime sdJd
              (_, JD utT') = splitToDayAndTime utJd'
              hasAmbigity = utT' < toDecimalHours (TimeOfDay 0 3 57)
              eps = 0.0000001
          in (hasAmbigity || abs(utN-utN') < eps) && (truncate utD) == (truncate sdD)
