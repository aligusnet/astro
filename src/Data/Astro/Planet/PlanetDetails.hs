{-|
Module: Data.Astro.Planet.PlanetDetails
Description: Planet Details
Copyright: Alexander Ignatyev, 2016-2017

Planet Details.
-}

module Data.Astro.Planet.PlanetDetails
(
  Planet(..)
  , PlanetDetails(..)
  , j2010PlanetDetails
  , isInnerPlanet
)

where

import Data.Astro.Types (DecimalDegrees(..), AstronomicalUnits, fromDMS)
import Data.Astro.Time.JulianDate (JulianDate)
import Data.Astro.Time.Epoch (j2010)


-- | Planets of the Solar System
data Planet = Mercury
             | Venus
             | Earth 
             | Mars
             | Jupiter
             | Saturn
             | Uranus
             | Neptune
               deriving (Show, Eq)


-- | Details of the planetary orbit at the epoch
data PlanetDetails = PlanetDetails {
  pdPlanet :: Planet
  , pdEpoch :: JulianDate
  , pdTp :: Double               -- ^ Orbital period in tropical years
  , pdEpsilon :: DecimalDegrees  -- ^ Longitude at the Epoch
  , pdOmegaBar :: DecimalDegrees -- ^ Longitude of the perihelion
  , pdE :: Double                -- ^ Eccentricity of the orbit
  , pdAlpha :: AstronomicalUnits -- ^ Semi-major axis of the orbit in AU
  , pdI :: DecimalDegrees        -- ^ Orbital inclination
  , pdBigOmega :: DecimalDegrees -- ^ Longitude of the ascending node
  , pdBigTheta :: DecimalDegrees -- ^ Angular diameter at 1 AU
  } deriving (Show, Eq)


-- | Return True if the planet is inner (its orbit lies inside the Earth's orbit)
isInnerPlanet :: PlanetDetails -> Bool
isInnerPlanet pd
  | pdPlanet pd == Mercury = True
  | pdPlanet pd == Venus = True
  | otherwise = False


-- | PlanetDetails at the reference Epoch J2010.0
j2010PlanetDetails :: Planet -> PlanetDetails
--                                                 Epoch Tp         Epsilon    Omega Bar  e        alpha    i        Big Omega Big Theta
j2010PlanetDetails Mercury = PlanetDetails Mercury j2010 0.24085    75.5671    77.612     0.205627 0.387098 7.0051   48.449    (arcsecs 6.74)
j2010PlanetDetails Venus   = PlanetDetails Venus   j2010 0.615207   272.30044  131.54     0.006812 0.723329 3.3947   76.769    (arcsecs 16.92)
j2010PlanetDetails Earth   = PlanetDetails Earth   j2010 0.999996   99.556772  103.2055   0.016671 0.999985 0        0         (arcsecs 0)
j2010PlanetDetails Mars    = PlanetDetails Mars    j2010 1.880765   109.09646  336.217    0.093348 1.523689 1.8497   49.632    (arcsecs 9.36)
j2010PlanetDetails Jupiter = PlanetDetails Jupiter j2010 11.857911  337.917132 14.6633    0.048907 5.20278  1.3035   100.595   (arcsecs 196.74)
j2010PlanetDetails Saturn  = PlanetDetails Saturn  j2010 29.310579  172.398316 89.567     0.053853 9.51134  2.4873   113.752   (arcsecs 165.6)
j2010PlanetDetails Uranus  = PlanetDetails Uranus  j2010 84.039492  356.135400 172.884833 0.046321 19.21814 0.773059 73.926961 (arcsecs 65.8)
j2010PlanetDetails Neptune = PlanetDetails Neptune j2010 165.845392 326.895127 23.07      0.010483 30.1985  1.7673   131.879   (arcsecs 62.2)

-- | arcseconds to DecimalHours
arcsecs = fromDMS 0 0
