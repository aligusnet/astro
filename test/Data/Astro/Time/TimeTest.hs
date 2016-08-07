module Data.Astro.Time.TimeTest
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

import Data.Astro.Time
import Data.Astro.Time.Time

tests = [testGroup "to decimal hours" [
            testCase "6:00" $ toDecimalHours (TimeOfDay 6 0 0) @?= 6.0
            , testCase "18:00" $ toDecimalHours (TimeOfDay 18 0 0) @?= 18.0
            , testCase "18:30" $ toDecimalHours (TimeOfDay 18 30 0) @?= (18*2 + 1) / 2
            , testCase "00:00:30" $ toDecimalHours (TimeOfDay 0 0 30) @?= 30 / (60*60)
            , testCase "00:00:10" $ assertApproxEqual "" 0.00000001 (10/(60*60)) $ toDecimalHours (TimeOfDay 0 0 10)
            , testCase "23:59:59.99999" $ assertApproxEqual "" 0.00000001 24.0 $ toDecimalHours (TimeOfDay 23 59 59.99999)
            ]
        , testGroup "from decimal hours" [
            testCase "6:00" $ fromDecimalHours 6.0  @?= TimeOfDay 6 0 0
            , testCase "18:00" $ fromDecimalHours 18.0 @?= TimeOfDay 18 0 0
            , testCase "18:30" $ fromDecimalHours  ((18*2 + 1) / 2) @?= TimeOfDay 18 30 0
            , testCase "00:00:30" $ fromDecimalHours (30 / (60*60)) @?= TimeOfDay 0 0 30
            ]
        , testGroup "decimal hours conversion properties" [
            testProperty "" prop_decimalHoursConversion 
            ]
        ]

prop_decimalHoursConversion =
  forAll (choose (0, 1.0)) $ checkDecimalHoursConversionProperties

checkDecimalHoursConversionProperties :: Double -> Bool
checkDecimalHoursConversionProperties n =
  let n2 = toDecimalHours $ fromDecimalHours n
  in abs (n-n2) < 0.00000001
