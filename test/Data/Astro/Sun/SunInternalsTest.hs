module Data.Astro.Sun.SunInternalsTest
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

import Data.Astro.Sun.SunInternals

tests = [testGroup "solveKeplerEquation" [
            testCase "a" $ assertApproxEqual ""
              1e-7
              3.5220041
              (solveKeplerEquation 0.016714 3.528210 1e-7)
            ]
        ]
