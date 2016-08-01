module Data.Astro.CalendarTest
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

import Data.Astro.Calendar

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
        , testGroup "from time" [
            testCase "6:00" $ fromTime (TimeOfDay 6 0 0) @?= 0.25
            , testCase "18:00" $ fromTime (TimeOfDay 18 0 0) @?= 0.75
            , testCase "18:30" $ fromTime (TimeOfDay 18 30 0) @?= (18*2 + 1) / 48
            , testCase "00:00:30" $ fromTime (TimeOfDay 0 0 30) @?= 30 / (60*60*24)
            , testCase "00:00:10" $ assertApproxEqual "" 0.00000001 (10/(60*60*24)) $ fromTime (TimeOfDay 0 0 10)
            , testCase "23:59:59.99999" $ assertApproxEqual "" 0.00000001 1.0 $ fromTime (TimeOfDay 23 59 59.99999)
            ]
        , testGroup "to julian day" [
            testCase "19 Jun 2009 18:00" (fromDateTime (LocalTime (fromGregorian 2009 6 19) (TimeOfDay 18 0 0)) @?= JulianDayNumber 2455002.25)
            , testCase "1 Aug 2009 12:00" (fromDateTime (LocalTime (fromGregorian 2016 8 1) (TimeOfDay 12 0 0)) @?= JulianDayNumber 2457602)
            , testCase "Gregorian start day" (fromDateTime (LocalTime (fromGregorian 1582 10 15) (TimeOfDay 0 0 0)) @?= JulianDayNumber 2299160.5)
            , testCase "Gregorian before start" (fromDateTime (LocalTime (fromGregorian 1582 10 14) (TimeOfDay 12 0 0)) @?= JulianDayNumber 2299170)
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
