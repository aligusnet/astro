{-|
Module: Data.Astro.Effects
Description: Physical effects
Copyright: Alexander Ignatyev, 2016

Physical effects which influence on accuracy of astronomical calculations.
-}

module Data.Astro.Effects
(
  refract
)

where

import Data.Astro.Types (DecimalDegrees(..), toRadians)

-- | Calculate the atmospheric refraction's angle.
-- It takes the observed altitude (of Horizon Coordinates), temperature in degrees centigrade and barometric pressure in millibars.
refract :: DecimalDegrees -> Double -> Double -> DecimalDegrees
refract altitude temperature pressure =
  let f = if altitude > (DD 15) then refractHighAltitude else refractLowAltitude
  in f altitude temperature pressure


refractHighAltitude :: DecimalDegrees -> Double -> Double -> DecimalDegrees
refractHighAltitude altitude temperature pressure =
  let z = toRadians $ 90 - altitude  -- zenith angle
  in DD $ 0.00452*pressure*(tan z) /(273+temperature) 


refractLowAltitude :: DecimalDegrees -> Double -> Double -> DecimalDegrees
refractLowAltitude altitude temperature pressure =
  let a = toRadians altitude
  in DD $ pressure*(0.1594+0.0196*a+0.00002*a*a)/((273+temperature)*(1+0.505*a+0.0845*a*a))
