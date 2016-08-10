module Data.Astro.EffectsTest
(
  tests
)

where


import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

import Data.Astro.TypesTest (testDecimalDegrees)

import Data.Astro.Types (DecimalDegrees(..))
import Data.Astro.Effects

tests = [testGroup "refraction" [
            testDecimalDegrees ""
              0.000001
              (DD 0.045403)
              (refract (DD 19.334345) 13 1008)
            ]
        ]
