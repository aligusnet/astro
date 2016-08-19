module Data.Astro.Planet.PlanetDetailsTest
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

import Data.Astro.Planet.PlanetDetails

tests = [testGroup "inner planets" [
            testCase "Mercury" $ isInnerPlanet (j2010PlanetDetails Mercury) @?= True
            , testCase "Venus" $ isInnerPlanet (j2010PlanetDetails Venus) @?= True
            , testCase "Jupiter" $ isInnerPlanet (j2010PlanetDetails Jupiter) @?= False
            , testCase "Uranus" $ isInnerPlanet (j2010PlanetDetails Uranus) @?= False
            , testCase "Earth" $ isInnerPlanet (j2010PlanetDetails Earth) @?= False
            ]
        ]
