module Data.Astro.TimeTest
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

import Data.Time.Calendar (fromGregorian, toGregorian)
import Control.Monad (unless)

import Data.Astro.Time

tests = [testGroup "easter day" [
            testCase "2009" easterDay2009
            , testCase "2016" easterDay2016
            , testCase "2027" easterDay2027
            , testProperty "easter properties" prop_Easter
            ]
         , testGroup "leap year" [
             testCase "2016" (isLeapYear 2016 @?= True)
             , testCase "2000" (isLeapYear 2000 @?= True)
             , testCase "2015" (isLeapYear 2015 @?= False)
             , testCase "1900" (isLeapYear 1900 @?= False)
             , testCase "1800" (isLeapYear 1800 @?= False)
           ]
         , testGroup "day number" [
             testCase "1 Jan of leap year" (dayNumber (fromGregorian 2016 1 1) @?= 1)
             , testCase "11 Feb of leap year" (dayNumber (fromGregorian 2016 2 11) @?= 42)
             , testCase "10 Mar of leap year" (dayNumber (fromGregorian 2016 3 10) @?= 70)
             , testCase "15 Nov of leap year" (dayNumber (fromGregorian 2016 11 15) @?= 320)
             , testCase "31 Dec of leap year" (dayNumber (fromGregorian 2016 12 31) @?= 366)
             , testCase "1 Jan of non-leap year" (dayNumber (fromGregorian 2015 1 1) @?= 1)
             , testCase "11 Feb of non-leap year" (dayNumber (fromGregorian 2015 2 11) @?= 42)
             , testCase "10 Mar of non-leap year" (dayNumber (fromGregorian 2015 3 10) @?= 69)
             , testCase "15 Nov of non-leap year" (dayNumber (fromGregorian 2015 11 15) @?= 319)
             , testCase "31 Dec of non-leap year" (dayNumber (fromGregorian 2015 12 31) @?= 365)
             ]
        , testGroup "to decimal hours" [
            testCase "6:00" $ toDecimalHours (TimeOfDay 6 0 0) @?= 0.25
            , testCase "18:00" $ toDecimalHours (TimeOfDay 18 0 0) @?= 0.75
            , testCase "18:30" $ toDecimalHours (TimeOfDay 18 30 0) @?= (18*2 + 1) / 48
            , testCase "00:00:30" $ toDecimalHours (TimeOfDay 0 0 30) @?= 30 / (60*60*24)
            , testCase "00:00:10" $ assertApproxEqual "" 0.00000001 (10/(60*60*24)) $ toDecimalHours (TimeOfDay 0 0 10)
            , testCase "23:59:59.99999" $ assertApproxEqual "" 0.00000001 1.0 $ toDecimalHours (TimeOfDay 23 59 59.99999)
            ]
        , testGroup "from decimal hours" [
            testCase "6:00" $ fromDecimalHours 0.25  @?= TimeOfDay 6 0 0
            , testCase "18:00" $ fromDecimalHours 0.75 @?= TimeOfDay 18 0 0
            , testCase "18:30" $ fromDecimalHours  ((18*2 + 1) / 48) @?= TimeOfDay 18 30 0
            , testCase "00:00:30" $ fromDecimalHours (30 / (60*60*24)) @?= TimeOfDay 0 0 30
            ]
        , testGroup "decimal hours conversion properties" [
            testProperty "" prop_decimalHoursConversion 
            ]
        , testGroup "to julian day" [
            testCase "19 Jun 2009 18:00" (fromDateTime (LocalTime (fromGregorian 2009 6 19) (TimeOfDay 18 0 0)) @?= JulianDayNumber 2455002.25)
            , testCase "1 Aug 2009 12:00" (fromDateTime (LocalTime (fromGregorian 2016 8 1) (TimeOfDay 12 0 0)) @?= JulianDayNumber 2457602)
            , testCase "Gregorian start day" (fromDateTime (LocalTime (fromGregorian 1582 10 15) (TimeOfDay 0 0 0)) @?= JulianDayNumber 2299160.5)
            , testCase "Gregorian before start" (fromDateTime (LocalTime (fromGregorian 1582 10 14) (TimeOfDay 12 0 0)) @?= JulianDayNumber 2299170)
            ]
        , testGroup "from julian day" [
            testCase "19 Jun 2009 18:00" (toDateTime (JulianDayNumber 2455002.25) @?= LocalTime (fromGregorian 2009 6 19) (TimeOfDay 18 0 0))
            , testCase "1 Aug 2009 12:00" (toDateTime (JulianDayNumber 2457602) @?= LocalTime (fromGregorian 2016 8 1) (TimeOfDay 12 0 0))
            , testCase "Gregorian start day" (toDateTime (JulianDayNumber 2299160.5) @?= LocalTime (fromGregorian 1582 10 15) (TimeOfDay 0 0 0))
            , testCase "Gregorian before start" (toDateTime (JulianDayNumber 2299160) @?= LocalTime (fromGregorian 1582 10 4) (TimeOfDay 12 0 0))
            ]
        , testGroup "julian conversion properties" [
            testProperty "before George" prop_JulianConversionsBeforeGeorge
          , testProperty "after George" prop_JulianConversionsAfterGeorge
          ]
        , testGroup "day of the week" [
            testCase "friday at midnight" $ dayOfWeek (JulianDayNumber 2455001.5) @?= 5
            , testCase "friday before moon" $ dayOfWeek (JulianDayNumber 2455001.75) @?= 5
            , testCase "friday after moon" $ dayOfWeek (JulianDayNumber 2455002.25) @?= 5
            , testCase "thursday after moon" $ dayOfWeek (JulianDayNumber 2455001.3) @?= 4
            , testCase "sunday at midnight" $ dayOfWeek (JulianDayNumber 2455003.5) @?= 0
          ]
        , testGroup "spliToDayAndTime" [
            testCase "100000.5" $ splitToDayAndTime (JulianDayNumber 100000.5) @?= (JulianDayNumber 100000.5, JulianDayNumber 0)
            , testCase "100000.7" $ assertJDPair 0.00001 (JulianDayNumber 100000.5, JulianDayNumber 0.2) $ splitToDayAndTime (JulianDayNumber 100000.7)
            , testCase "100001.3" $ assertJDPair 0.00001 (JulianDayNumber 100000.5, JulianDayNumber 0.8) $ splitToDayAndTime (JulianDayNumber 100001.3)
            , testCase "2444352.108931"
                $ assertJDPair 0.00001 (JulianDayNumber 2444351.5, JulianDayNumber 0.608931)
                $ splitToDayAndTime (JulianDayNumber 2444352.108931)
            , testProperty "property" prop_splitToDayAndTime
            ]
        , testGroup "siderealTime" [
            testJulianDayNumber "1980-04-22 14:36:51.67 UT"
                0.0000001
                (JulianDayNumber 2444351.694504972)
                (toSiderealTime $ JulianDayNumber 2444352.108931366)
            , testJulianDayNumber "2016-08-04 19:28:43.15 UT"
                0.0000001
                (JulianDayNumber 2457605.183251656)
                (toSiderealTime $ JulianDayNumber 2457605.3116105325)
            , testJulianDayNumber "1980-04-22 04:40:05.23 GST"
                0.0000001
                (JulianDayNumber 2444352.108931366)
                (fromSiderealTime $ JulianDayNumber 2444351.694504972)
            , testJulianDayNumber "2016-08-04 16:23:52.84 GST"
                0.0000001
                (JulianDayNumber 2457605.3116105325)
                (fromSiderealTime $ JulianDayNumber 2457605.183251656)
            , testProperty "property" prop_siderealTimeConversions
            ]
        ]

easterDay2009 = easterDayInYear 2009 @?= fromGregorian 2009 4 12
easterDay2016 = easterDayInYear 2016 @?= fromGregorian 2016 3 27
easterDay2027 = easterDayInYear 2027 @?= fromGregorian 2027 3 28

prop_Easter =
  forAll (choose (1583, 999900)) $ checkEasterProperties

checkEasterProperties year = let date = easterDayInYear year
                                 (y, m, _) = toGregorian date
                             in fromIntegral y == year && (m == 3 || m == 4)


prop_decimalHoursConversion =
  forAll (choose (0, 1.0)) $ checkDecimalHoursConversionProperties

checkDecimalHoursConversionProperties :: Double -> Bool
checkDecimalHoursConversionProperties n =
  let n2 = toDecimalHours $ fromDecimalHours n
  in abs (n-n2) < 0.00000001

prop_JulianConversionsAfterGeorge =
  forAll (choose (2299161, 999999999)) $ checkJulianConverionProperties


prop_JulianConversionsBeforeGeorge =
  forAll (choose (0, 2299161)) $ checkJulianConverionProperties


checkJulianConverionProperties :: Double -> Bool
checkJulianConverionProperties n =
  let jd = JulianDayNumber n
      dt = toDateTime jd
      jd2 = fromDateTime dt
      JulianDayNumber n2 = jd2
  in abs(n - n2) < 0.00000001

assertJDPair :: Double -> (JulianDayNumber, JulianDayNumber) -> (JulianDayNumber, JulianDayNumber) -> Assertion
assertJDPair eps e@(JulianDayNumber e1, JulianDayNumber e2) a@(JulianDayNumber a1, JulianDayNumber a2) = 
  unless (abs(a1-e1) <= eps && abs(a2-e2) <= eps) (assertFailure msg)
  where msg = "expected: " ++ show e ++ "\n but got: " ++ show a ++
              "\n (maximum margin of error: " ++ show eps ++ ")"

prop_splitToDayAndTime =
  forAll (choose (0, 999999999)) $ check
  where check jd =
          let (JulianDayNumber d, JulianDayNumber t) = splitToDayAndTime $ JulianDayNumber jd
              eps = 0.0000001
          in abs(jd-d-t) < eps && t >= 0 && t < 1 


testJulianDayNumber msg eps expected actual =
  testCase msg $ assertJulianDayNumber eps expected actual

assertJulianDayNumber eps (JulianDayNumber expected) (JulianDayNumber actual) =
  unless (abs(expected-actual) <= eps) (assertFailure msg)
  where msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual ++
              "\n (maximum margin of error: " ++ show eps ++ ")"

prop_siderealTimeConversions =
  forAll (choose (0, 999999999)) $ check
  where check utN =
          let utJd = JulianDayNumber utN
              sdJd = toSiderealTime utJd
              utJd'@(JulianDayNumber utN') = fromSiderealTime sdJd
              (JulianDayNumber utD, _) = splitToDayAndTime utJd
              (JulianDayNumber sdD, _) = splitToDayAndTime sdJd
              (_, JulianDayNumber utT') = splitToDayAndTime utJd'
              hasAmbigity = utT' < toDecimalHours (TimeOfDay 0 3 57)
              eps = 0.0000001
          in (hasAmbigity || abs(utN-utN') < eps) && (truncate utD) == (truncate sdD)
