module Data.Astro.Planet.PlanetMechanicsTest
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

import Data.Astro.Types (DecimalDegrees(..), DecimalHours(..), fromDMS)
import Data.Astro.Time.JulianDate (JulianDate(..))
import Data.Astro.Time.Epoch (j2010)
import Data.Astro.Coordinate (EquatorialCoordinates1(..))
import Data.Astro.Planet.PlanetDetails (Planet(..), j2010PlanetDetails)
import Data.Astro.Planet.PlanetMechanics

nov222003 = JD 2452965.5
jupiterDetails = j2010PlanetDetails Jupiter
earthDetails = j2010PlanetDetails Earth
mercuryDetails = j2010PlanetDetails Mercury

tests = [testGroup "mechanics" [
            testDecimalDegrees "jupiter mean anomaly"
                0.0000001
                137.8097641
                (planetMeanAnomaly jupiterDetails nov222003)
            , testDecimalDegrees "jupiter true anomaly"
                0.0000001
                141.5736000
                (planetTrueAnomaly1 jupiterDetails nov222003)
            , testDecimalDegrees "planetHeliocentricLongitude"
                0.0000001
                114.6633
                (planetHeliocentricLongitude jupiterDetails 100)
            , testDecimalDegrees "planetHeliocentricLatitude"
                0.0000001
                1.1220101
                (planetHeliocentricLatitude jupiterDetails 160)
            , testCase "planetHeliocentricRadiusVector" $ assertApproxEqual ""
                0.0000001
                5.4403612
                (planetHeliocentricRadiusVector jupiterDetails 160)
            , testDecimalDegrees "planetProjectedLongitude"
                0.0000001
                159.9935029
                (planetProjectedLongitude jupiterDetails 160)
            , testCase "planetProjectedRadiusVector" $ assertApproxEqual ""
                0.0000001
                5.4387668
                (planetProjectedRadiusVector jupiterDetails 1.22 5.44)
            , testDecimalDegrees "planetEclipticLongitude: Jupiter"
                0.0000001
                169.793671
                (planetEclipticLongitude jupiterDetails 160 5.43 59 0.988)
            , testDecimalDegrees "planetEclipticLongitude: Mercury"
                0.000001
                253.929762
                (planetEclipticLongitude mercuryDetails 287.824406 0.448159 59.274748 0.987847)
            , testDecimalDegrees "planetEclipticLatitude: Jupiter"
                0.0000001
                1.1861236
                (planetEclipticLatitude 1.22 160 5.43 59 0.988 170)
            , testDecimalDegrees "planetEclipticLatitude: Mercury"
                0.000001
                (-2.044058)
                (planetEclipticLatitude (-6.035842) 287.824406 0.448159 59.274748 0.987847 253.929762)
            , testEC1 "planetPosition1: Jupiter"
                0.0000001
                (EC1 (DD 6.3569686) (DH 11.1871664))
                (planetPosition1 jupiterDetails earthDetails nov222003)
            , testEC1 "planetPosition1: Mercury"
                0.0000001
                (EC1 (DD (-24.5023748)) (DH 16.8200600))
                (planetPosition1 mercuryDetails earthDetails nov222003)
            , testEC1 "planetPosition"
                0.0000001
                (planetPosition1 jupiterDetails earthDetails nov222003)
                (planetPosition planetTrueAnomaly1 jupiterDetails earthDetails nov222003)
             , testDecimalDegrees "pertubations: Jupiter"
                0.0000001
                0.0371214
                (planetPertubations Jupiter nov222003)
             , testDecimalDegrees "pertubations: Saturn"
                0.0000001
                0.1317895  -- not sure if this is the right result
                (planetPertubations Saturn nov222003)
             , testDecimalDegrees "pertubations: Mars"
                0.0000001
                0
                (planetPertubations Mars nov222003)
             , testCase "planetDistance1" $ assertApproxEqual ""
                0.0000001
                5.6033062
                (planetDistance1 jupiterDetails earthDetails nov222003)
             , testDecimalDegrees "planetAngularDiameter"
                0.0000001
                (fromDMS 0 0 35.1114)
                (planetAngularDiameter jupiterDetails 5.6033062)
            ]
         , testGroup "planetPhase" [
             testCase "Jupiter" $ assertApproxEqual ""
                0.0000001
                0.9922919
                (planetPhase1 jupiterDetails earthDetails nov222003)
             , testCase "Mercury" $ assertApproxEqual ""
                0.0000001
                0.9141158
                (planetPhase1 mercuryDetails earthDetails nov222003)
             ]
        ]
