module Data.Astro.Time.EpochTest
(
  tests
)

where

import Data.Astro.Time.Epoch
import Data.Astro.Time.JulianDate

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.HUnit.Approx


tests = [ testGroup "Epoch" [
            testCase "B1900" $ b1900 @?= jdAdd (fromYMDHMS 1900 1 0 0 0 0) 0.8135
          , testCase "B1950" $ b1950 @?= jdAdd (fromYMDHMS 1950 1 0 0 0 0) 0.9235
          , testCase "J1900" $ j1900 @?= fromYMDHMS 1900 1 0 12 0 0
          , testCase "J2000" $ j2000 @?= fromYMDHMS 2000 1 1 12 0 0
          , testCase "J2050" $ j2050 @?= fromYMDHMS 2050 1 1 0 0 0
          , testCase "J2010" $ j2010 @?= fromYMDHMS 2010 1 0 0 0 0
          ]
        ]

jdAdd (JD jd) days = JD (jd + days)