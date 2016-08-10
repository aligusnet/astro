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
  let z = toRadians $ 90 - altitude  -- zenith angle
  in DD $ 0.00452*pressure*(tan z) /(273+temperature) 
