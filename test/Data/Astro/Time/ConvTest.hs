module Data.Astro.Time.ConvTest
(
  tests
)

where

import Data.Astro.Types
import Data.Time.LocalTime
import Data.Astro.Time.Conv
import Data.Astro.Time.JulianDate
import Data.Astro.Time.JulianDateTest (testJD)

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.HUnit.Approx
import Test.QuickCheck


tests = [ testGroup "LCT conversion properties" [
            testProperty "TZ: -5" $ prop_LCTConversion 4
            , testProperty "TZ: 0" $ prop_LCTConversion 0
            , testProperty "TZ: 3" $ prop_LCTConversion 3
            ]
          , testGroup "LCD Conversion properties" [
              testProperty "TZ: -5" $ prop_LCDConversion 4
              , testProperty "TZ: 0" $ prop_LCDConversion 0
              , testProperty "TZ: 3" $ prop_LCDConversion 3
              ]
          ]

prop_LCTConversion tz = forAll (choose (0, 999999999)) check
  where check n = 
          let jd = LCT (DH tz) (JD n)
              jd2 = zonedTimeToLCT $ lctToZonedTime jd
              LCT _ (JD n2) = jd2
          in abs(n - n2) < 0.00000001


prop_LCDConversion tz = forAll (choose (0, 999999999)) check
  where check n = 
          let (jd, _) = splitToDayAndTime (JD n)  -- drop time part
              lct = LCT (DH tz) jd
              lcd2 = zonedTimeToLCD $ lctToZonedTime lct
              LCD _ (JD n2) = lcd2
              JD n1 = jd
          in abs(n1 - n2) < 0.00000001

