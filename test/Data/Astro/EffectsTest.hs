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
            testDecimalDegrees "19.33"
                0.000001
                (DD 0.045403)
                (refract (DD 19.334345) 13 1008)
            , testDecimalDegrees "horizon"
                0.000001
                (DD 0.566569)
                (refract (DD 0) 12 1013)
            , testDecimalDegrees "azimuth"
                0.000001
                (DD 0)
                (refract (DD 90) 12 1012)
            , testDecimalDegrees "azimuth"
                0.000001
                (DD 0.007905)
                (refract (DD 63.5) 15.5 1012)
            ]
        ]
