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

import Data.Astro.Utils (reduceToZeroRange)
import Data.Astro.Sun.SunInternals

tests = [testGroup "solveKeplerEquation" [
            testCase "a" $ assertApproxEqual ""
              1e-7
              3.5220041
              (solveKeplerEquation 0.016714 3.528210 1e-7)
            , testProperty "property" prop_solveKeplerEquation
            ]
        ]

prop_solveKeplerEquation (e, m) =
  let eps = 1e-7
      (_, e') = properFraction e
      e'' =( e' * 0.01) + 0.1
      m' = (reduceToZeroRange 17 e) - 7
      x = solveKeplerEquation e'' m' eps
      dx = x - e''*(sin x) - m'
  in abs dx < eps
  where types = ((e,m)::(Double, Double))
