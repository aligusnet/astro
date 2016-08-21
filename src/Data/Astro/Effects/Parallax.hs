{-|
Module: Data.Astro.Effects.Parallax
Description: Calculation effects of geocentric parallax
Copyright: Alexander Ignatyev, 2016


Calculation effects of geocentric parallax.
-}

module Data.Astro.Effects.Parallax
(
  parallaxQuantities
  , parallax
)

where

import Data.Astro.Types (DecimalDegrees(..)
                        , DecimalHours(..)
                        , AstronomicalUnits(..)
                        , GeographicCoordinates(..)
                        , toRadians, fromRadians
                        , fromDMS
                        , toDecimalHours, fromDecimalHours)
import Data.Astro.Time (utToLST)
import Data.Astro.Time.JulianDate (JulianDate(..))
import Data.Astro.Time.Sidereal (LocalSiderealTime(..), utToGST, gstToLST)
import Data.Astro.Coordinate (EquatorialCoordinates1(..), raToHA)


-- | It takes latitude of the observer
-- and height above sea-level of the observer measured in metres
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


-- | Calculate the apparent position of the celestial object (the Sun or a planet).
-- It takes geocraphic coordinates of the observer and height above sea-level of the observer measured in metres,
-- distance from the celestial object to the Earth measured in AU, the Universal Time and geocentric equatorial coordinates.
-- It returns adjusted equatorial coordinates.
parallax :: GeographicCoordinates -> Double -> AstronomicalUnits -> JulianDate -> EquatorialCoordinates1 -> EquatorialCoordinates1
parallax (GeoC latitude longitude) height distance ut (EC1 delta alpha) =
  let piD = earthRadiusUnitsAU distance
      lst = utToLST longitude ut
      (pSin, pCos) = parallaxQuantities latitude height
      ha = toRadians $ fromDecimalHours $ raToHA alpha longitude ut
      delta' = toRadians delta
      dAlpha = (toDecimalHours piD) * (DH $ (sin ha)*pCos/(cos delta'))
      dDelta = piD * (DD $ pSin*(cos delta') - pCos*(cos ha)*(sin delta'))
  in EC1 (delta-dDelta) (alpha-dAlpha)


-- | It takes the distance in metres and
-- returns the distance measured in units of qquatorial Earth radius
earthRadiusUnits :: Double -> Double
earthRadiusUnits d = d / 6378140


--earthRadiusUnitsAU :: AstronomicalUnits -> DecimalDegrees
earthRadiusUnitsAU (AU d) = fromDMS 0 0 (8.794/d)
