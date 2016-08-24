{-|
Module: Data.Astro.Moon
Description: Calculation characteristics of the Moon
Copyright: Alexander Ignatyev, 2016

Calculation characteristics of the Moon.

-}

module Data.Astro.Moon
(
  moonPosition1
)

where

import qualified Data.Astro.Utils as U
import Data.Astro.Types (DecimalDegrees(..), toRadians, fromRadians)
import Data.Astro.Time.JulianDate (JulianDate(..), numberOfDays)
import Data.Astro.Coordinate (EquatorialCoordinates1(..), EclipticCoordinates(..), eclipticToEquatorial)
import Data.Astro.Sun (sunDetails, sunMeanAnomaly2, sunEclipticLongitude2)
import Data.Astro.Moon.MoonDetails (MoonDetails(..))


-- | Reduce the value to the range [0, 360)
reduceDegrees :: DecimalDegrees -> DecimalDegrees
reduceDegrees = U.reduceToZeroRange 360


-- | Calculate Equatorial Coordinates of the Moon with the given MoonDetails and at the given JulianDate.
-- It is recommended to use 'j2010MoonDetails' as a first parameter.
moonPosition1 :: MoonDetails -> JulianDate -> EquatorialCoordinates1
moonPosition1 md ut =
  let d = DD $ numberOfDays (mdEpoch md) ut
      sd = sunDetails ut
      ms = toRadians $ sunMeanAnomaly2 sd
      lambdaS = sunEclipticLongitude2 sd
      lm = reduceDegrees $ (mdL md) + 13.1763966*d  -- Moon's mean longitude
      mm = reduceDegrees $ lm - 0.1114041*d - (mdP md)  -- Moon's mean anomaly
      nm = reduceDegrees $ (mdN md) - 0.0529539*d  -- ascending node's mean longitude
      c = lm - lambdaS
      ev = 1.2739 * (sin $ toRadians $ 2*c - mm)  -- correction for evection
      ae = 0.1858 * (sin ms)  -- correction for annual equation
      a3 = 0.37 * (sin ms)  -- third correction
      mm' = toRadians $ mm + (DD $ ev - ae - a3) -- Moon's corrected anomaly
      ec = 6.2886 * (sin mm')  -- correction for the equation of the centre
      a4 = 0.214 * (sin $ 2*mm') -- fourth correction term
      lm' = lm + (DD $ ev + ec -ae + a4) -- Moon's corrected longitude
      v = DD $ 0.6583 * (sin $ toRadians $ 2*(lm' - lambdaS))-- correction for variation
      lm'' = lm' + v -- true longitude
      nm' = nm - (DD $ 0.16 * (sin ms)) -- ascending node's corrected longitude
      a = toRadians $ lm''-nm'
      i = toRadians $ mdI md
      y = (sin a) * (cos i)
      x = cos a
      at = reduceDegrees $ fromRadians $ atan2 y x
      lambdaM = at + nm'
      betaM = fromRadians $ asin $ (sin a) * (sin i)
  in eclipticToEquatorial (EcC betaM lambdaM) ut
