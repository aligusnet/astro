{-|
Module: Data.Astro.Planet.PlanetMechanics
Description: Planet mechanics
Copyright: Alexander Ignatyev, 2016

Planet mechanics.
-}

module Data.Astro.Planet.PlanetMechanics
(
  planetMeanAnomaly
  , planetTrueAnomaly1
  , planetHeliocentricRadiusVector
  , planetHeliocentricLongitude
  , planetHeliocentricLatitude
  , planetProjectedRadiusVector
  , planetProjectedLongitude
  , planetEclipticLongitude
  , planetEclipticLatitude
  , planetPosition
  , planetPosition1
  , planetDistance1
  , planetAngularDiameter
  , planetPhase1
  , planetPertubations
  , planetPositionAngle
)

where

import qualified Data.Astro.Utils as U
import Data.Astro.Types (DecimalDegrees(..), AstronomicalUnits(..), toRadians, fromRadians, fromDecimalHours)
import Data.Astro.Time.Epoch (j1900)
import Data.Astro.Time.JulianDate (JulianDate, numberOfDays, numberOfCenturies)
import Data.Astro.Coordinate (EquatorialCoordinates1(..), EclipticCoordinates(..), eclipticToEquatorial)
import Data.Astro.Planet.PlanetDetails (Planet(..), PlanetDetails(..), isInnerPlanet)
import Data.Astro.Sun.SunInternals (solveKeplerEquation)

{-
1. Calculate the planet position on its own orbital plane
2. Convert the planet's position to planetHeliocentric coordinates.
3. Convert from planetHeliocentric coordinates to ecliptic coordinates.
-}


-- | reduce DecimalDegrees to the range [0, 360)
reduceDegrees :: DecimalDegrees -> DecimalDegrees
reduceDegrees = U.reduceToZeroRange 360


-- | Calculate the planet mean anomaly.
planetMeanAnomaly pd jd =
  let d =  numberOfDays (pdEpoch pd) jd
      n = reduceDegrees $ DD $ (360/U.tropicalYearLen) * (d/(pdTp pd))
  in reduceDegrees $ n + (pdEpsilon pd) - (pdOmegaBar pd)


-- | Calculate the planet true anomaly using approximate method
planetTrueAnomaly1 pd jd =
  let meanAnomaly = toRadians $ planetMeanAnomaly pd jd
      e = pdE pd
  in reduceDegrees $ fromRadians $ meanAnomaly + 2*e*(sin meanAnomaly)


-- | Calculate Heliocentric Longitude.
-- It takes Planet Details and true anomaly.
planetHeliocentricLongitude :: PlanetDetails -> DecimalDegrees -> DecimalDegrees
planetHeliocentricLongitude pd trueAnomaly = reduceDegrees $ (pdOmegaBar pd) + trueAnomaly


-- | Calculate Heliocentric Latitude.
-- It takes Planet Details and heliocentric longitude.
planetHeliocentricLatitude :: PlanetDetails -> DecimalDegrees -> DecimalDegrees
planetHeliocentricLatitude pd hcl =
  let l' = toRadians hcl
      i' = toRadians $ pdI pd
      bigOmega' = toRadians $ pdBigOmega pd
  in fromRadians $ asin $ (sin $ l' - bigOmega')*(sin i')


-- | Calculate Heliocentric Radius Vector.
-- It takes Planet Details and true anomaly.
planetHeliocentricRadiusVector :: PlanetDetails -> DecimalDegrees -> AstronomicalUnits
planetHeliocentricRadiusVector pd trueAnomaly =
  let nu = toRadians trueAnomaly
      AU alpha = pdAlpha pd
      e = pdE pd
  in AU $ alpha*(1 - e*e)/(1+e*(cos nu))


-- | Calculate Heliocentric Longitude projected to the ecliptic.
-- It takes Planet Details and Heliocentric Longitude
planetProjectedLongitude :: PlanetDetails -> DecimalDegrees -> DecimalDegrees
planetProjectedLongitude pd hcl =
  let hcl' = toRadians hcl
      bigOmega = pdBigOmega pd
      bigOmega' = toRadians $ bigOmega
      i' = toRadians $ pdI pd
      y = (sin $ hcl'-bigOmega')*(cos i')
      x = (cos $ hcl'-bigOmega')
      n = fromRadians $ atan2 y x
  in n + bigOmega


-- | Calculate Heliocentric Radius Vector projected to the ecliptic.
-- It takes Planet Details, planetHeliocentric latitude and Radius Vector 
planetProjectedRadiusVector :: PlanetDetails -> DecimalDegrees -> AstronomicalUnits -> AstronomicalUnits
planetProjectedRadiusVector pd psi (AU hcr) = AU $ hcr*cos(toRadians psi)


-- | Calculate ecliptic longitude for outer planets.
-- It takes planet projected longitude, planet projected radius vector
-- the Earth's longitude and radius vector.
outerPlanetEclipticLongitude :: DecimalDegrees -> AstronomicalUnits -> DecimalDegrees -> AstronomicalUnits -> DecimalDegrees
outerPlanetEclipticLongitude lp (AU rp) le (AU re) =
  let lp' = toRadians lp
      le' = toRadians le
      x = atan $ re * (sin $ lp'-le')/(rp - re*(cos $ lp'-le'))
  in reduceDegrees $ (fromRadians x) + lp


-- | Calculate ecliptic longitude for inner planets.
-- It takes planet projected longitude, planet projected radius vector
-- the Earth's longitude and radius vector.
innerPlanetEclipticLongitude :: DecimalDegrees -> AstronomicalUnits -> DecimalDegrees -> AstronomicalUnits -> DecimalDegrees
innerPlanetEclipticLongitude lp (AU rp) le (AU re) =
  let lp' = toRadians lp
      le' = toRadians le
      x = atan $ rp * (sin $ le'-lp')/(re - rp*(cos $ le'-lp'))
  in reduceDegrees $ (fromRadians x) + le + 180


-- | Calculate Ecliptic Longitude.
-- It takes planet projected longitude, planet projected radius vector
-- the Earth's longitude and radius vector.
planetEclipticLongitude :: PlanetDetails -> DecimalDegrees -> AstronomicalUnits -> DecimalDegrees -> AstronomicalUnits -> DecimalDegrees
planetEclipticLongitude pd
  | isInnerPlanet pd = innerPlanetEclipticLongitude
  | otherwise = outerPlanetEclipticLongitude


-- | Calculate ecliptic Latitude.
-- It takes the planet's: heliocentric latitude, projected heliocentric longutide,
-- projected heliocentric longitude;
-- the Earth's: heliocentric longitede and heliocentric radius vector.
-- Also it takes the planet's ecliptic longitude. 
planetEclipticLatitude :: DecimalDegrees
                          -> DecimalDegrees
                          -> AstronomicalUnits
                          -> DecimalDegrees
                          -> AstronomicalUnits
                          -> DecimalDegrees
                          -> DecimalDegrees
planetEclipticLatitude psi lp (AU rp) le (AU re) lambda =
  let psi' = toRadians psi
      lp' = toRadians lp
      le' = toRadians le
      lambda' = toRadians lambda
      y = rp*(tan psi')*(sin $ lambda' - lp')
      x = re * (sin $ lp' -le')
  in fromRadians $ atan (y/x)


-- | Calculate the planet's postion at the given date.
-- It takes a function to calculate true anomaly,
-- planet details of the planet, planet details of the Earth
-- and JulianDate.
planetPosition :: (PlanetDetails -> JulianDate -> DecimalDegrees)
                  -> PlanetDetails -> PlanetDetails -> JulianDate
                  -> EquatorialCoordinates1
planetPosition trueAnomaly pd ed jd =
      -- planet
  let nup = trueAnomaly pd jd
      lp = planetHeliocentricLongitude pd nup
      rp = planetHeliocentricRadiusVector pd nup
      psi = planetHeliocentricLatitude pd lp
      lp' = planetProjectedLongitude pd lp
      rp' = planetProjectedRadiusVector pd psi rp
      -- earth
      nue = trueAnomaly ed jd
      le = planetHeliocentricLongitude ed nue
      re = planetHeliocentricRadiusVector ed nue
      -- position
      lambda = planetEclipticLongitude pd lp' rp' le re
      beta = planetEclipticLatitude psi lp' rp' le re lambda
      ec = eclipticToEquatorial (EcC beta lambda) jd
    in ec


-- | Calculates the distance betweeth the planet and the Earth at the given date.
-- It takes the planet's detail, the Earth's details and the julian date.
planetDistance1 :: PlanetDetails -> PlanetDetails -> JulianDate -> AstronomicalUnits
planetDistance1 pd ed jd =
  let nup = planetTrueAnomaly1 pd jd
      lp = planetHeliocentricLongitude pd nup
      AU rp = planetHeliocentricRadiusVector pd nup
      psi = planetHeliocentricLatitude pd lp
      -- earth
      nue = planetTrueAnomaly1 ed jd
      le = planetHeliocentricLongitude ed nue
      AU re = planetHeliocentricRadiusVector ed nue
      -- distance
      ro = sqrt $ re*re + rp*rp - 2*re*rp*(cos . toRadians $ lp - le)*(cos $ toRadians psi)
    in AU ro


-- | Calculates the planet's angulat diameter for the given distance.
planetAngularDiameter :: PlanetDetails -> AstronomicalUnits -> DecimalDegrees
planetAngularDiameter pd (AU ro) = (pdBigTheta pd)/(DD ro)


-- | Calculate the planet's phase at the given phase.
-- Phase is a fraction of the visible disc that is illuminated.
-- It takes the planet's details, the Earth's details and the julian date.
-- Returns fraction values from 0 to 1.
planetPhase1 :: PlanetDetails -> PlanetDetails -> JulianDate -> Double
planetPhase1 pd ed jd =
      -- planet
  let nup = planetTrueAnomaly1 pd jd
      lp = planetHeliocentricLongitude pd nup
      rp = planetHeliocentricRadiusVector pd nup
      psi = planetHeliocentricLatitude pd lp
      lp' = planetProjectedLongitude pd lp
      rp' = planetProjectedRadiusVector pd psi rp
      -- earth
      nue = planetTrueAnomaly1 ed jd
      le = planetHeliocentricLongitude ed nue
      re = planetHeliocentricRadiusVector ed nue
      
      lambda = planetEclipticLongitude pd lp' rp' le re
      d = toRadians $ lambda - lp
    in (1+ (cos d)) * 0.5


-- | Calculate the planet's postion at the given date using the approximate algoruthm.
-- It takes a function to calculate true anomaly,
-- planet details of the planet, planet details of the Earth
-- and JulianDate.
planetPosition1 :: PlanetDetails -> PlanetDetails -> JulianDate
                  -> EquatorialCoordinates1
planetPosition1 = planetPosition planetTrueAnomaly1


-- | Calculates pertubations for the planet at the given julian date.
-- Returns a value that should be added to the mean longitude (planet heliocentric longitude).
planetPertubations :: Planet -> JulianDate -> DecimalDegrees
planetPertubations Jupiter jd =
  let (a, _, v, _) = pertubationsQuantities jd
      v' = toRadians v
      dl = (0.3314-0.0103*a)*(sin v') - 0.0644*a*(cos v')
  in DD dl
planetPertubations Saturn jd =
  let (a, q, v, b) = pertubationsQuantities jd
      q' = toRadians q
      v' = toRadians v
      b' = toRadians b
      dl = (0.1609*a-0.0105)*(cos v') + (0.0182*a-0.8142)*(sin v') - 0.1488*(sin b')
        - 0.0408*(sin $ 2*b') + 0.0856*(sin b')*(cos q') + 0.0813*(cos b')*(sin q')
  in DD dl
planetPertubations _ _ = 0


-- pertrubationsQuantities :: JulianDate
pertubationsQuantities jd =
  let t = numberOfCenturies j1900 jd
      a = t*0.2 + 0.1
      p = DD $ 237.47555 + 3034.9061*t
      q = DD $ 265.91650 + 1222.1139*t
      v = 5*q - 2*p
      b = q - p
  in (a, q, v, b)


-- | Calculate the planet's position-angle of the bright limb.
-- It takes the planet's coordinates and the Sun's coordinates.
planetPositionAngle :: EquatorialCoordinates1 -> EquatorialCoordinates1 -> DecimalDegrees
planetPositionAngle (EC1 deltaP alphaP) (EC1 deltaS alphaS) =
  let dAlpha = toRadians $ fromDecimalHours $ alphaS - alphaP
      deltaP' = toRadians deltaP
      deltaS' = toRadians deltaS
      y = (cos deltaS')*(sin dAlpha)
      x = (cos deltaP')*(sin deltaS') - (sin deltaP')*(cos deltaS')*(cos dAlpha)
      chi = reduceDegrees $ fromRadians $ atan2 y x
  in chi
