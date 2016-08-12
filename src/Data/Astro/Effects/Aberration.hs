{-|
Module: Data.Astro.Effects.Aberration
Description: Calculation effects of aberration.
Copyright: Alexander Ignatyev, 2016

Calculation effects of aberration.
-}

module Data.Astro.Effects.Aberration
(
  includeAberration
)

where

import Data.Astro.Types (DecimalDegrees, toRadians, fromDMS)
import Data.Astro.Time.JulianDate (JulianDate)
import Data.Astro.Coordinate (EclipticCoordinates(..))


-- | Includes aberration effect.
-- It takes true Ecliptic Coordinates,
-- the Sun's longitude at the given Julian Day (the third parameter).
-- Returns apparent ecliptic coordinates.
-- The Sun's longitude can be calculated using 'sunEclipticLongitude1' or 'sunEclipticLongitude2' of "Data.Astro.Sun" module.
includeAberration :: EclipticCoordinates -> JulianDate -> DecimalDegrees -> EclipticCoordinates
includeAberration (EcC beta lambda) jd sunLambda =
  let lambdaDiff = toRadians $ sunLambda - lambda
      beta' = toRadians beta
      dLambda = -20.5 * (cos lambdaDiff) / (cos beta')
      dBeta = -20.5 * (sin lambdaDiff) * (sin beta')
  in EcC (beta + fromDMS 0 0 dBeta) (lambda + fromDMS 0 0 dLambda)
