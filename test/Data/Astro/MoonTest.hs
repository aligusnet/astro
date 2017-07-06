module Data.Astro.MoonTest
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

import Data.Astro.TypesTest (testDecimalDegrees)
import Data.Astro.CoordinateTest (testEC1)

import Data.Astro.Types (GeographicCoordinates(..))
import Data.Astro.Time.JulianDate (fromYMD)
import Data.Astro.Coordinate (EquatorialCoordinates1(..))
import Data.Astro.Moon.MoonDetails (MoonDetails(..), j2010MoonDetails, MoonDistanceUnits(..))
import Data.Astro.Moon

tests = [testGroup "moonPosition1" [
            testEC1 "at 2003-09-01 00:00:00 UT"
                0.000001
                (EC1 (-11.525750) 14.211486)
                (moonPosition1 j2010MoonDetails (fromYMD 2003 9 1))
            ]
         , testGroup "moonPosition2" [
            testEC1 "at 2003-09-01 00:00:00 UT"
                0.000001
                (EC1 (-12.174888) 14.178731)
                (moonPosition2 j2010MoonDetails (MDU 1) (GeoC 51 0) 20 (fromYMD 2003 9 1))
            ]
         , testGroup "moonDistance" [
            testMDU "at 2016-08-27 00:00:00"
                0.000001
                (MDU 0.953425)
                (moonDistance1 j2010MoonDetails (fromYMD 2016 8 26))
            ]
         , testGroup "moonAngularSize" [
             testDecimalDegrees "at 0.953425 MDU"
                0.000001
                0.543409
                (moonAngularSize (MDU 0.953425))
             , testDecimalDegrees "at 1 MDU"
                0.000001
                (mdBigTheta j2010MoonDetails)
                (moonAngularSize (MDU 1))
             ]
         , testGroup "moonHorizontalParallax" [
             testDecimalDegrees "at 0.953425 MDU"
                0.000001
                0.997142
                (moonHorizontalParallax (MDU 0.953425))
             , testDecimalDegrees "at 1 MDU"
                0.000001
                (mdPi j2010MoonDetails)
                (moonHorizontalParallax (MDU 1))
             ]
         , testGroup "moonPhase" [
            testCase "at 2016-08-01 00:00:00" $ assertApproxEqual ""
                0.000001
                0.042498
                (moonPhase j2010MoonDetails (fromYMD 2016 8 1))
            , testCase "at 2016-08-21 00:00:00" $ assertApproxEqual ""
                0.000001
                0.911818
                (moonPhase j2010MoonDetails (fromYMD 2016 8 21))
            ]
         , testGroup "moonBrightLimbPositionAngle" [
             testDecimalDegrees "at 2016-08-28 00:00:00"
                0.000001
                82.479138
                (moonBrightLimbPositionAngle (EC1 17.386905 4.897826) (EC1 10.329324 10.342354))
             ]
        ]

testMDU msg eps (MDU e) (MDU a) = testCase msg $ assertApproxEqual "" eps e a
