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

import Data.Astro.Time.JulianDateTest (testJD)
import Data.Astro.Time
import Data.Astro.Time.JulianDate (JulianDate(..))
import Data.Astro.Types

tests = [testGroup "LCT <-> LST" [
            testJD "1980-04-22 14:36:51.67 LCT -> 1980-04-22 04:24:44.65 LST"
              0.00001
              (JD 2444351.6838501696)
              (lctToLST (DD (-64.0)) (-4) (JD 2444352.108931366))
            ]
        ]
