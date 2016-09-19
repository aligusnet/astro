module Data.Astro.Time.JulianDateTest
(
  tests
  , testJD
  , testLCT
)

where


import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.HUnit.Approx
import Test.QuickCheck

import Control.Monad (unless)

import Data.Astro.Types (DecimalHours(..))
import Data.Astro.Time.Epoch (b1950, j2000)
import Data.Astro.Time.JulianDate

tests = [testGroup "to julian day" [
            testCase "19 Jun 2009 18:00" $ fromYMDHMS 2009 6 19 18 0 0 @?= JD 2455002.25
            , testCase "1 Aug 2009 12:00" $ fromYMDHMS 2016 8 1 12 0 0 @?= JD 2457602
            , testCase "Gregorian start day" $ fromYMDHMS 1582 10 15 0 0 0 @?= JD 2299160.5
            , testCase "Gregorian before start" $ fromYMDHMS 1582 10 14 12 0 0 @?= JD 2299170
            , testCase "19 Jun 2009" $ fromYMD 2009 6 19 @?= JD 2455001.5
            , testCase "1 Aug 2009" $ fromYMD 2016 8 1 @?= JD 2457601.5
            ]
        , testGroup "from julian day" [
            testCase "19 Jun 2009 18:00" (toYMDHMS (JD 2455002.25) @?= (2009, 6, 19, 18, 0, 0))
            , testCase "1 Aug 2009 12:00" (toYMDHMS (JD 2457602) @?= (2016, 8, 1, 12, 0, 0))
            , testCase "Gregorian start day" (toYMDHMS (JD 2299160.5) @?= (1582, 10, 15, 0, 0, 0))
            , testCase "Gregorian before start" (toYMDHMS (JD 2299160) @?= (1582, 10, 4, 12, 0, 0))
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
        , testGroup "UT <-> LCT" [
            testLCT "2016-08-07 02:10:10 +4 LCT -> UT"
                0.000000001
                (LCT 4 (JD 2457607.423726852))
                (lctFromYMDHMS 4 2016 8 7 2 10 10)
            ]
        , testGroup "numberOfCenturies" [
            testCase "J2000..2009-06-06" $ assertApproxEqual ""
              0.000000001
              0.095099247
              (numberOfCenturies j2000 $ JD 2455018.5)
            ]
        , testGroup "numberOfYears" [
            testCase "B1950..1979-07-02" $ assertApproxEqual ""
              0.000000001
              29.5
              (numberOfYears b1950 $ JD 2444057.2985)
            ]
        , testGroup "numberOfDays" [
            testCase "10 .. 15" $ assertApproxEqual ""
              0.000000001
              5
              (numberOfDays 10 15)
            ]
        , testGroup "add hours" [
            testJD "+ 12H"
                0.000000001
                (JD 110.5)
                (addHours 12 110)
            , testJD "+ 0H"
                0.000000001
                (JD 110)
                (addHours 0 110)
            , testJD "- 6H"
                0.000000001
                (JD 109.75)
                (addHours (-6) 110)
            ]
        , testJD "julian start date" 0.000001 (JD 0) julianStartDateTime
        , testGroup "print" [
            testCase "show" $ "1999-09-19 19:29:59.0000 -2.0" @=? show (lctFromYMDHMS (-2) 1999 9 19 19 29 59)
            , testCase "printLtcHs" $ "lctFromYMDHMS (-2) 1999 9 19 19 29 59.0000" @=? printLctHs (lctFromYMDHMS (-2) 1999 9 19 19 29 59)
            ]
        , testCase "lctToYMDHMS" $ (1999, 9, 19, 12, 0, 0)  @=? lctToYMDHMS (lctFromYMDHMS (-2) 1999 9 19 12 0 0)
        , testGroup "JD: Num instance" [
            testCase "+" $ (JD 17.5) @=? (JD 15.5) + (JD 2)
            , testCase "-" $ (JD 13.5) @=? (JD 15.5) - (JD 2)
            , testCase "*" $ (JD 31) @=? (JD 15.5) * (JD 2)
            , testCase "negate" $ (JD 15.5) @=? negate (JD $ -15.5)
            , testCase "abs" $ (JD 15.7) @=? abs (JD (-15.7))
            , testCase "signum > 0" $ (JD 1.0) @=? signum (JD 15.5)
            , testCase "signum = 0" $ (JD 0.0) @=? signum (JD 0.0)
            , testCase "signum < 0" $ (JD $ -1.0) @=? signum (JD $ -15.5)
            , testCase "fromInteger" $ (JD 17) @=? fromInteger 17
            ]
        ]

prop_JulianConversionsAfterGeorge =
  forAll (choose (2299161, 999999999)) $ checkJulianConverionProperties


prop_JulianConversionsBeforeGeorge =
  forAll (choose (0, 2299161)) $ checkJulianConverionProperties


checkJulianConverionProperties :: Double -> Bool
checkJulianConverionProperties n =
  let jd = JD n
      (y, m, d, hs, ms, ss) = toYMDHMS jd
      jd2 = fromYMDHMS y  m  d  hs  ms ss
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

testJD msg eps expected actual =
  testCase msg $ assertJD eps expected actual

assertJD eps (JD expected) (JD actual) =
  unless (abs(expected-actual) <= eps) (assertFailure msg)
  where msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual ++
              "\n (maximum margin of error: " ++ show eps ++ ")"

testLCT msg eps expected actual =
  testCase msg $ assertLCT eps expected actual


assertLCT eps expected actual =
  unless (eqLCT eps expected actual) (assertFailure msg)
  where msg = "expected: " ++ show expected ++ "\nbut got: " ++ show actual ++
              "\n (maximum margin of error: " ++ show eps ++ ")"

eqLCT eps (LCT (DH tze) (JD jde)) (LCT (DH tza) (JD jda)) = abs (jde-jda) < eps && abs(tze-tza) < eps
