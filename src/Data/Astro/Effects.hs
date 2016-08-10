{-|
Module: Data.Astro.Effects
Description: Physical effects
Copyright: Alexander Ignatyev, 2016

Physical effects which influence on accuracy of astronomical calculations.
-}

module Data.Astro.Effects
(
  AstronomyEpoch(..)
  , refract
  , precession1
)

where

import Data.Astro.Types (DecimalDegrees(..), DecimalHours(..), toDecimalHours, fromDecimalHours, toRadians, fromRadians)
import Data.Astro.Time.JulianDate (JulianDate(..), b1900, b1950, j2000, j2050, numberOfYears)
import Data.Astro.Coordinate (EquatorialCoordinates1(..))


-------------------------------------------------------------------------------
-- Refraction

-- | Calculate the atmospheric refraction angle.
-- It takes the observed altitude (of Horizon Coordinates), temperature in degrees centigrade and barometric pressure in millibars.
-- The average sea level atmospheric pressure is 1013 millibars.
refract :: DecimalDegrees -> Double -> Double -> DecimalDegrees
refract altitude temperature pressure =
  let f = if altitude > (DD 15) then refractBigAlpha else refractSmallAlpha
  in f altitude temperature pressure


-- | Calculate the atmospheric refraction angle for big values of alpha (altitude) (> 15 decimal degrees)
refractBigAlpha :: DecimalDegrees -> Double -> Double -> DecimalDegrees
refractBigAlpha altitude temperature pressure =
  let z = toRadians $ 90 - altitude  -- zenith angle
  in DD $ 0.00452*pressure*(tan z) /(273+temperature) 


-- | Calculate the atmospheric refraction angle for small values of alpha (altitude)
refractSmallAlpha :: DecimalDegrees -> Double -> Double -> DecimalDegrees
refractSmallAlpha altitude temperature pressure =
  let a = toRadians altitude
  in DD $ pressure*(0.1594+0.0196*a+0.00002*a*a)/((273+temperature)*(1+0.505*a+0.0845*a*a))


-------------------------------------------------------------------------------
-- Precession

-- | Epoch Enumeration. See also "Data.Astro.Time.JulianDate" module.
data AstronomyEpoch = B1900  -- ^ Epoch B1900.0
                    | B1950  -- ^ Epoch B1950.0
                    | J2000  -- ^ Epoch J2000.0
                    | J2050  -- ^ Epoch J2050.0
                    deriving (Show, Eq)


-- | Get the start date of the specified Epoch.
epochToJD :: AstronomyEpoch -> JulianDate
epochToJD B1900 = b1900
epochToJD B1950 = b1950
epochToJD J2000 = j2000
epochToJD J2050 = j2050


-- | Precisional Constants
data PrecessionalConstants = PrecessionalConstants {
  pcM :: Double     -- ^ seconds
  , pcN :: Double   -- ^ seconds
  , pcN' :: Double  -- ^ arcsec
  }


-- | Get Precision Constants of the Epoch
precessionalConstants :: AstronomyEpoch -> PrecessionalConstants
precessionalConstants B1900 = PrecessionalConstants 3.07234 1.33645 20.0468
precessionalConstants B1950 = PrecessionalConstants 3.07327 1.33617 20.0426
precessionalConstants J2000 = PrecessionalConstants 3.07420 1.33589 20.0383
precessionalConstants J2050 = PrecessionalConstants 3.07513 1.33560 20.0340


-- | Low-precision method to calculate luni-solar precession.
-- It takes Epoch, Equatorial Coordinates and Julian Date of the observation.
-- It returns corrected Equatorial Coordinates.
precession1 :: AstronomyEpoch -> EquatorialCoordinates1 -> JulianDate -> EquatorialCoordinates1
precession1 epoch (EC1 delta alpha) jd =
  let delta' = toRadians delta
      alpha' = toRadians $ fromDecimalHours alpha
      years = numberOfYears (epochToJD epoch) jd
      PrecessionalConstants m n n' = precessionalConstants epoch
      s1 = DH $ (m + n*(sin alpha')*(tan delta'))*years / 3600
      s2 = DD $ (n'*(cos alpha')) * years / 3600
  in (EC1 (delta + s2) (alpha + s1))
