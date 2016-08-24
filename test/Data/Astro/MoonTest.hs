module Data.Astro.MoonTest
(
  tests
)

where


import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

import Data.Astro.CoordinateTest (testEC1)

import Data.Astro.Time.JulianDate (fromYMD)
import Data.Astro.Coordinate (EquatorialCoordinates1(..))
import Data.Astro.Moon.MoonDetails (j2010MoonDetails)
import Data.Astro.Moon

tests = [testGroup "moonPosition1"[
            testEC1 "at 2003-09-01 00:00:00 UT"
                0.000001
                (EC1 (-11.525750) 14.211486)
                (moonPosition1 j2010MoonDetails (fromYMD 2003 9 1))
            ]
        ]
