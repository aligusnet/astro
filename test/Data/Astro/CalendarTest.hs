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

tests = [testGroup "easterDay" [
            testCase "2009" easterDay2009
            , testCase "2016" easterDay2016
            , testCase "2027" easterDay2027
            , testProperty "easter properties" prop_Easter
            ]
        ]

easterDay2009 = easterDayInYear 2009 @?= Date 2009 4 12
easterDay2016 = easterDayInYear 2016 @?= Date 2016 3 27
easterDay2027 = easterDayInYear 2027 @?= Date 2027 3 28

prop_Easter =
  forAll (choose (1583, 999900)) $ checkEasterProperties

checkEasterProperties year = let date = easterDayInYear year
                           in getYear date == year && (getMonth date == 3 || getMonth date == 4)
