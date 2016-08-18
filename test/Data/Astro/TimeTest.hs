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

import Data.Astro.Time.SiderealTest (testLST)
import Data.Astro.Time.JulianDateTest (testJD, testLCT)
import Data.Astro.Time
import Data.Astro.Time.JulianDate (JulianDate(..), LocalCivilTime(..), lctFromYMDHMS, lcdFromYMD)
import Data.Astro.Types
import Data.Astro.Time.Sidereal

tests = [testGroup "LCT <-> LST" [
            testLST "1980-04-22 14:36:51.67 LCT -> 1980-04-22 04:24:44.65 LST"
                0.0000001
                (hmsToLST 4 24 44.655)
                (lctToLST (DD (-64.0)) (LCT (-4) (JD 2444352.2755980324)))
            , testLCT "1980-04-22 04:24:44.65 LST -> 1980-04-22 14:36:51.67 LCT"
                0.001
                (lctFromYMDHMS (-4) 1980 4 22 14 35 51.6704)
                (lstToLCT (DD (-64.0)) (lcdFromYMD (-4) 1980 4 22) (hmsToLST 04 24 44.655))
            ]
        ]
