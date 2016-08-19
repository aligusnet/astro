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
)

where

import qualified Data.Astro.Utils as U
import Data.Astro.Types (DecimalDegrees(..), toRadians, fromRadians)
import Data.Astro.Time.JulianDate (JulianDate, numberOfDays)
import Data.Astro.Coordinate (EquatorialCoordinates1, EclipticCoordinates(..), eclipticToEquatorial)
import Data.Astro.Planet.PlanetDetails (Planet, PlanetDetails(..), isInnerPlanet)

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
planetHeliocentricRadiusVector :: PlanetDetails -> DecimalDegrees -> Double
planetHeliocentricRadiusVector pd trueAnomaly =
  let nu = toRadians trueAnomaly
      alpha = pdAlpha pd
      e = pdE pd
  in alpha*(1 - e*e)/(1+e*(cos nu))


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
planetProjectedRadiusVector :: PlanetDetails -> DecimalDegrees -> Double -> Double
planetProjectedRadiusVector pd psi hcr = hcr*cos(toRadians psi)


-- | Calculate ecliptic longitude for outer planets.
-- It takes planet projected longitude, planet projected radius vector
-- the Earth's longitude and radius vector.
outerPlanetEclipticLongitude :: DecimalDegrees -> Double -> DecimalDegrees -> Double -> DecimalDegrees
outerPlanetEclipticLongitude lp rp le re =
  let lp' = toRadians lp
      le' = toRadians le
      x = atan $ re * (sin $ lp'-le')/(rp - re*(cos $ lp'-le'))
  in reduceDegrees $ (fromRadians x) + lp


-- | Calculate ecliptic longitude for inner planets.
-- It takes planet projected longitude, planet projected radius vector
-- the Earth's longitude and radius vector.
innerPlanetEclipticLongitude :: DecimalDegrees -> Double -> DecimalDegrees -> Double -> DecimalDegrees
innerPlanetEclipticLongitude lp rp le re =
  let lp' = toRadians lp
      le' = toRadians le
      x = atan $ rp * (sin $ le'-lp')/(re - rp*(cos $ le'-lp'))
  in reduceDegrees $ (fromRadians x) + le + 180


-- | Calculate Ecliptic Longitude.
-- It takes planet projected longitude, planet projected radius vector
-- the Earth's longitude and radius vector.
planetEclipticLongitude :: PlanetDetails -> DecimalDegrees -> Double -> DecimalDegrees -> Double -> DecimalDegrees
planetEclipticLongitude pd
  | isInnerPlanet pd = innerPlanetEclipticLongitude
  | otherwise = outerPlanetEclipticLongitude


-- | Calculate ecliptic Latitude.
-- It takes the planet's: heliocentric latitude, projected heliocentric longutide,
-- projected heliocentric longitude;
-- the Earth's: heliocentric longitede and heliocentric radius vector.
-- Also it takes the planet's ecliptic longitude. 
planetEclipticLatitude :: DecimalDegrees -> DecimalDegrees -> Double -> DecimalDegrees -> Double -> DecimalDegrees -> DecimalDegrees
planetEclipticLatitude psi lp rp le re lambda =
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
      nue = planetTrueAnomaly1 ed jd
      le = planetHeliocentricLongitude ed nue
      re = planetHeliocentricRadiusVector ed nue
      -- position
      lambda = planetEclipticLongitude pd lp' rp' le re
      beta = planetEclipticLatitude psi lp' rp' le re lambda
      ec = eclipticToEquatorial (EcC beta lambda) jd
    in ec


-- | Calculate the planet's postion at the given date using the approximate algoruthm.
-- It takes a function to calculate true anomaly,
-- planet details of the planet, planet details of the Earth
-- and JulianDate.
planetPosition1 :: PlanetDetails -> PlanetDetails -> JulianDate
                  -> EquatorialCoordinates1
planetPosition1 = planetPosition planetTrueAnomaly1
