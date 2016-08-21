module Data.Astro.Effects.ParallaxTest
(
  tests
)

where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.HUnit.Approx

import Control.Monad (unless)

import Data.Astro.Effects.Parallax

tests = [testGroup "parallaxQuantities" [
            testPairOfDoubles "DD 50, 60 metres"
                0.000001
                (0.762422, 0.644060)
                (parallaxQuantities 50 60)
            , testPairOfDoubles "DD -70, 10 metres"
                0.000001
                (-0.936174, 0.3430361)
                (parallaxQuantities (-70) 10)
            ]
        ]


testPairOfDoubles msg eps expected actual =
  testCase msg $ assertPairOfDoubles eps expected actual

assertPairOfDoubles eps expected@(e1, e2) actual@(a1, a2) =
  unless (abs(e1-a1) <= eps && abs(e2-a2) <= eps) (assertFailure msg)
  where msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual ++
              "\n (maximum margin of error: " ++ show eps ++ ")"
