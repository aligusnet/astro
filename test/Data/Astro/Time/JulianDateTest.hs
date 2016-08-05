module Data.Astro.Time.JulianDateTest
(
  tests
)

where


import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

import Data.Time.Calendar (fromGregorian, toGregorian)
import Control.Monad (unless)

import Data.Astro.Time (TimeOfDay(..), LocalTime(..))
import Data.Astro.Time.JulianDate

tests = [testGroup "to julian day" [
            testCase "19 Jun 2009 18:00" (fromDateTime (LocalTime (fromGregorian 2009 6 19) (TimeOfDay 18 0 0)) @?= JD 2455002.25)
            , testCase "1 Aug 2009 12:00" (fromDateTime (LocalTime (fromGregorian 2016 8 1) (TimeOfDay 12 0 0)) @?= JD 2457602)
            , testCase "Gregorian start day" (fromDateTime (LocalTime (fromGregorian 1582 10 15) (TimeOfDay 0 0 0)) @?= JD 2299160.5)
            , testCase "Gregorian before start" (fromDateTime (LocalTime (fromGregorian 1582 10 14) (TimeOfDay 12 0 0)) @?= JD 2299170)
            ]
        , testGroup "from julian day" [
            testCase "19 Jun 2009 18:00" (toDateTime (JD 2455002.25) @?= LocalTime (fromGregorian 2009 6 19) (TimeOfDay 18 0 0))
            , testCase "1 Aug 2009 12:00" (toDateTime (JD 2457602) @?= LocalTime (fromGregorian 2016 8 1) (TimeOfDay 12 0 0))
            , testCase "Gregorian start day" (toDateTime (JD 2299160.5) @?= LocalTime (fromGregorian 1582 10 15) (TimeOfDay 0 0 0))
            , testCase "Gregorian before start" (toDateTime (JD 2299160) @?= LocalTime (fromGregorian 1582 10 4) (TimeOfDay 12 0 0))
            ]
        , testGroup "julian conversion properties" [
            testProperty "before George" prop_JulianConversionsBeforeGeorge
          , testProperty "after George" prop_JulianConversionsAfterGeorge
          ]
        , testGroup "day of the week" [
            testCase "friday at midnight" $ dayOfWeek (JD 2455001.5) @?= 5
            , testCase "friday before moon" $ dayOfWeek (JD 2455001.75) @?= 5
            , testCase "friday after moon" $ dayOfWeek (JD 2455002.25) @?= 5
            , testCase "thursday after moon" $ dayOfWeek (JD 2455001.3) @?= 4
            , testCase "sunday at midnight" $ dayOfWeek (JD 2455003.5) @?= 0
          ]
        , testGroup "spliToDayAndTime" [
            testCase "100000.5" $ splitToDayAndTime (JD 100000.5) @?= (JD 100000.5, JD 0)
            , testCase "100000.7" $ assertJDPair 0.00001 (JD 100000.5, JD 0.2) $ splitToDayAndTime (JD 100000.7)
            , testCase "100001.3" $ assertJDPair 0.00001 (JD 100000.5, JD 0.8) $ splitToDayAndTime (JD 100001.3)
            , testCase "2444352.108931"
                $ assertJDPair 0.00001 (JD 2444351.5, JD 0.608931)
                $ splitToDayAndTime (JD 2444352.108931)
            , testProperty "property" prop_splitToDayAndTime
            ]
        ]

prop_JulianConversionsAfterGeorge =
  forAll (choose (2299161, 999999999)) $ checkJulianConverionProperties


prop_JulianConversionsBeforeGeorge =
  forAll (choose (0, 2299161)) $ checkJulianConverionProperties


checkJulianConverionProperties :: Double -> Bool
checkJulianConverionProperties n =
  let jd = JD n
      dt = toDateTime jd
      jd2 = fromDateTime dt
      JD n2 = jd2
  in abs(n - n2) < 0.00000001

assertJDPair :: Double -> (JulianDate, JulianDate) -> (JulianDate, JulianDate) -> Assertion
assertJDPair eps e@(JD e1, JD e2) a@(JD a1, JD a2) = 
  unless (abs(a1-e1) <= eps && abs(a2-e2) <= eps) (assertFailure msg)
  where msg = "expected: " ++ show e ++ "\n but got: " ++ show a ++
              "\n (maximum margin of error: " ++ show eps ++ ")"

prop_splitToDayAndTime =
  forAll (choose (0, 999999999)) $ check
  where check jd =
          let (JD d, JD t) = splitToDayAndTime $ JD jd
              eps = 0.0000001
          in abs(jd-d-t) < eps && t >= 0 && t < 1
