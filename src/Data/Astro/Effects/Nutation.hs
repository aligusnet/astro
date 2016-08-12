{-|
Module: Data.Astro.Effects.Nutation
Description: Calculation effects of nutation
Copyright: Alexander Ignatyev, 2016

Calculation effects of nutation.
-}

module Data.Astro.Effects.Nutation
(
  nutationLongitude
  , nutationObliquity
)

where

import qualified Data.Astro.Utils as U
import Data.Astro.Types (DecimalDegrees(..), toRadians, fromDMS)
import Data.Astro.Time.JulianDate (JulianDate, j1900, numberOfCenturies)


-- | Calculates the nutation on the ecliptic longitude at the given JulianDate
nutationLongitude :: JulianDate -> DecimalDegrees
nutationLongitude jd =
  let t = numberOfCenturies j1900 jd
      l = sunMeanLongutude t
      omega = moonNode t
      dPsi = -17.2*(sin omega) - 1.3*(sin $ 2*l)
  in fromDMS 0 0 dPsi


-- | Calculates the nutation on the obliquity of the ecliptic at the given JulianDate
nutationObliquity :: JulianDate -> DecimalDegrees
nutationObliquity jd =
  let t = numberOfCenturies j1900 jd
      l = sunMeanLongutude t
      omega = moonNode t
      dEps = 9.2*(cos omega) + 0.5*(cos $ 2*l)
  in fromDMS 0 0 dEps


-- | It takes a number of centuries and returns the Sun's mean longitude in radians
sunMeanLongutude :: Double -> Double
sunMeanLongutude t =
  let a = 100.002136 * t
  in U.toRadians $ U.reduceToZeroRange 360 $ 279.6967 + 360 * (a - int a)


-- | It takes a number of centuries and returns the Moon's node in radians
moonNode :: Double -> Double
moonNode t =
  let b = 5.372617 * t
  in U.toRadians $ U.reduceToZeroRange 360 $ 259.1833 - 360*(b - int b)


-- | 'round' function that returns Double
int :: Double -> Double
int = fromIntegral . round
