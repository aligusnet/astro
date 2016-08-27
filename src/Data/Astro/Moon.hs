{-|
Module: Data.Astro.Moon
Description: Calculation characteristics of the Moon
Copyright: Alexander Ignatyev, 2016

Calculation characteristics of the Moon.

-}

module Data.Astro.Moon
(
  moonPosition1
  , moonDistance1
  , moonAngularSize
)

where

import qualified Data.Astro.Utils as U
import Data.Astro.Types (DecimalDegrees(..), toRadians, fromRadians)
import Data.Astro.Time.JulianDate (JulianDate(..), numberOfDays)
import Data.Astro.Coordinate (EquatorialCoordinates1(..), EclipticCoordinates(..), eclipticToEquatorial)
import Data.Astro.Sun (sunDetails, sunMeanAnomaly2, sunEclipticLongitude2)
import Data.Astro.Moon.MoonDetails (MoonDetails(..), MoonDistanceUnits(..), j2010MoonDetails)


-- | Reduce the value to the range [0, 360)
reduceDegrees :: DecimalDegrees -> DecimalDegrees
reduceDegrees = U.reduceToZeroRange 360


-- | Calculate Equatorial Coordinates of the Moon with the given MoonDetails and at the given JulianDate.
-- It is recommended to use 'j2010MoonDetails' as a first parameter.
moonPosition1 :: MoonDetails -> JulianDate -> EquatorialCoordinates1
moonPosition1 md ut =
  let sd = sunDetails ut
      lambdaS = sunEclipticLongitude2 sd
      ms = sunMeanAnomaly2 sd
      mmq = meanMoonQuantities md ut
      MQ lm'' _ nm' = correctedMoonQuantities lambdaS ms mmq
      a = toRadians $ lm''-nm'
      i = toRadians $ mdI md
      y = (sin a) * (cos i)
      x = cos a
      at = reduceDegrees $ fromRadians $ atan2 y x
      lambdaM = at + nm'
      betaM = fromRadians $ asin $ (sin a) * (sin i)
  in eclipticToEquatorial (EcC betaM lambdaM) ut


-- | Calculates the Moon's Distance at the given julian date.
-- Returns distance to the Moon
-- moonDistance1 :: JulianDate -> MoonDistanceUnits
-- you can use 'mduToKm' (defined in "Data.Astro.Moon.MoonDetails") to convert result to kilometers
moonDistance1 md ut =
  let sd = sunDetails ut
      lambdaS = sunEclipticLongitude2 sd
      ms = sunMeanAnomaly2 sd
      mmq = meanMoonQuantities md ut
      cmq = correctedMoonQuantities lambdaS ms mmq
      mm' = toRadians $ mqAnomaly cmq
      ec = toRadians $ centreEquation mm'
      e = mdE md
  in MDU $ (1 - e*e)/(1+e*(cos(mm'+ec)))


-- | Calculate the Moons' angular size at the given distance.
moonAngularSize :: MoonDistanceUnits -> DecimalDegrees
moonAngularSize (MDU p) = (mdBigTheta j2010MoonDetails) / (DD p)


-- | The Moon's quantities
-- Used to store intermidiate results
data MoonQuantities = MQ {
  mqLongitude :: DecimalDegrees        -- ^ the Moon's longitude
  , mqAnomaly :: DecimalDegrees        -- ^ the Moon's anomaly
  , mqAscendingNode :: DecimalDegrees  -- ^ the Moon's ascending node's longitude
  }


-- | Calculates the Moon's mean quantities on the given date.
-- It takes the Moon's orbita details and julian date
meanMoonQuantities :: MoonDetails -> JulianDate -> MoonQuantities
meanMoonQuantities md ut =
  let d = DD $ numberOfDays (mdEpoch md) ut
      lm = reduceDegrees $ (mdL md) + 13.1763966*d  -- Moon's mean longitude
      mm = reduceDegrees $ lm - 0.1114041*d - (mdP md)  -- Moon's mean anomaly
      nm = reduceDegrees $ (mdN md) - 0.0529539*d  -- ascending node's mean longitude
  in MQ lm mm nm


-- | Calculates correction for the equation of the centre
-- It takes the Moon's corrected anomaly in radians
centreEquation :: Double -> DecimalDegrees
centreEquation mm = DD $ 6.2886 * (sin mm)


-- | Calculates the Moon's corrected longitude, anomaly and asceding node's longitude
-- It takes the Sun's longitude, the Sun's mean anomaly and the Moon's mean quantities
correctedMoonQuantities :: DecimalDegrees -> DecimalDegrees -> MoonQuantities -> MoonQuantities
correctedMoonQuantities lambdaS ms (MQ lm mm nm) =
  let ms' = toRadians ms
      c = lm - lambdaS
      ev = DD $ 1.2739 * (sin $ toRadians $ 2*c - mm)  -- correction for evection
      ae = DD $ 0.1858 * (sin ms')  -- correction for annual equation
      a3 = DD $ 0.37 * (sin ms')  -- third correction
      mm' = mm + (ev - ae - a3) -- Moon's corrected anomaly
      mm'' = toRadians mm'
      ec = centreEquation mm''  -- correction for the equation of the centre
      a4 = DD $ 0.214 * (sin $ 2*mm'') -- fourth correction term
      lm' = lm + (ev + ec -ae + a4) -- Moon's corrected longitude
      v = DD $ 0.6583 * (sin $ toRadians $ 2*(lm' - lambdaS))-- correction for variation
      lm'' = lm' + v -- Moon's true orbital longitude
      nm' = nm - (DD $ 0.16 * (sin ms')) -- ascending node's corrected longitude
  in MQ lm'' mm' nm'
