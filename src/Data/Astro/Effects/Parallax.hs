{-|
Module: Data.Astro.Effects.Parallax
Description: Calculation effects of geocentric parallax
Copyright: Alexander Ignatyev, 2016


Calculation effects of geocentric parallax.
-}

module Data.Astro.Effects.Parallax
(
  parallaxQuantities
)

where

import Data.Astro.Types (DecimalDegrees, toRadians, fromRadians)


-- | It takes latitude of the observer
-- and height of the observed above sea-level measured in metres
-- Returns palallax quantities (p*(sin phi'), p*(cos phi')),
-- where phi' is the geocentric latitude
-- and p is the distance of the obserbve from the centre of the Earth.
parallaxQuantities :: DecimalDegrees -> Double -> (Double, Double)
parallaxQuantities latitude height =
  let c = 0.996647
      phi = toRadians latitude
      h = earthRadiusUnits height
      u = atan (c*(tan phi))
      pSin = c * (sin u) + h*(sin phi)
      pCos = (cos u) + h*(cos phi)
  in (pSin, pCos)


-- | It takes the distance in metres and
-- returns the distance measured in units of qquatorial Earth radius
earthRadiusUnits :: Double -> Double
earthRadiusUnits d = d / 6378140
