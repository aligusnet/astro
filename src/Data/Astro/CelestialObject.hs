{-|
Module: Data.Astro.CelestialObject
Description: Computations characteristics of selestial objects
Copyright: Alexander Ignatyev, 2016

Computations characteristics of selestial objects.
-}

module Data.Astro.CelestialObject
(
  angleEquatorial
  , angleEcliptic
)

where

import Data.Astro.Types (DecimalDegrees, toRadians, fromRadians, fromDecimalHours)
import Data.Astro.Coordinate (EquatorialCoordinates1(..), EclipticCoordinates(..))


-- | Calculate angle between two celestial objects
-- whose coordinates specified in Equatorial Coordinate System.
angleEquatorial :: EquatorialCoordinates1 -> EquatorialCoordinates1 -> DecimalDegrees
angleEquatorial (EC1 delta1 alpha1) (EC1 delta2 alpha2) =
  calcAngle (delta1, fromDecimalHours alpha1) (delta2, fromDecimalHours alpha2)


-- | Calculate angle between two celestial objects
-- whose coordinates specified in Ecliptic Coordinate System.
angleEcliptic :: EclipticCoordinates -> EclipticCoordinates -> DecimalDegrees
angleEcliptic (EcC beta1 lambda1) (EcC beta2 lambda2) =
  calcAngle (beta1, lambda1) (beta2, lambda2)


calcAngle :: (DecimalDegrees, DecimalDegrees) -> (DecimalDegrees, DecimalDegrees) -> DecimalDegrees
calcAngle (up1, round1) (up2, round2) =
  let up1' = toRadians up1
      round1' = toRadians round1
      up2' = toRadians up2
      round2' = toRadians round2
      d = acos $ (sin up1')*(sin up2') + (cos up1')*(cos up2')*cos(round1'-round2')
  in fromRadians d
