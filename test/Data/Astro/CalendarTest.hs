module Data.Astro.CalendarTest
(
  tests
)

where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

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
             testCase "1 Jan of leap year" (dayNumber (Date 2016 1 1) @?= 1)
             , testCase "11 Feb of leap year" (dayNumber (Date 2016 2 11) @?= 42)
             , testCase "10 Mar of leap year" (dayNumber (Date 2016 3 10) @?= 70)
             , testCase "15 Nov of leap year" (dayNumber (Date 2016 11 15) @?= 320)
             , testCase "31 Dec of leap year" (dayNumber (Date 2016 12 31) @?= 366)
             , testCase "1 Jan of non-leap year" (dayNumber (Date 2015 1 1) @?= 1)
             , testCase "11 Feb of non-leap year" (dayNumber (Date 2015 2 11) @?= 42)
             , testCase "10 Mar of non-leap year" (dayNumber (Date 2015 3 10) @?= 69)
             , testCase "15 Nov of non-leap year" (dayNumber (Date 2015 11 15) @?= 319)
             , testCase "31 Dec of non-leap year" (dayNumber (Date 2015 12 31) @?= 365)
             ]
        ]

easterDay2009 = easterDayInYear 2009 @?= Date 2009 4 12
easterDay2016 = easterDayInYear 2016 @?= Date 2016 3 27
easterDay2027 = easterDayInYear 2027 @?= Date 2027 3 28

prop_Easter =
  forAll (choose (1583, 999900)) $ checkEasterProperties

checkEasterProperties year = let date = easterDayInYear year
                           in getYear date == year && (getMonth date == 3 || getMonth date == 4)

