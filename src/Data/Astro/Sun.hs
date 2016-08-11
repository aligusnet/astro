{-|
Module: Data.Astro.Sun
Description: Calculation characteristics of the Sun
Copyright: Alexander Ignatyev, 2016

== Calculation characteristics of the Sun.

=== /Terms/

* __perihelion__ - minimal distance from the Sun to the planet
* __aphelion__ - maximal distance from the Sun to the planet

* __perigee__ - minimal distance from the Sun to the Earth
* __apogee__ - maximal distance from the Sun to the Earth

-}

module Data.Astro.Sun
(
  SunDetails(..)
  , j2010
  , sunDetails
)

where

import Data.Astro.Utils (reduceToZeroRange)
import Data.Astro.Types (DecimalDegrees(..))
import Data.Astro.Time.JulianDate (JulianDate(..), numberOfCenturies)


-- | Details of the Sun's apparent orbit at the given epoch
data SunDetails = SunDetails {
  sdEpoch :: JulianDate             -- ^ Epoch
  , sdEpsilon :: DecimalDegrees     -- ^ Ecliptic longitude at the Epoch
  , sdOmega :: DecimalDegrees       -- ^ Ecliptic longitude of perigee at the Epoch
  , sdE :: Double                   -- ^ Eccentricity oforbit at the Epoch
  } deriving (Show)


-- | Epoch J1900.0
j1900 :: JulianDate
j1900 = JD 2415020.0


-- | The Sun's reference Epoch J2010.0 (2010 January 0.0)
j2010 :: JulianDate
j2010 = JD 2455196.5


-- | SunDetails at the Sun's reference Epoch J2010.0
j2010SunDetails :: SunDetails
j2010SunDetails = SunDetails j2010 (DD 279.557208) (DD 283.112438) 0.016705


-- | Calculate SunDetails for the given JulianDate.
sunDetails :: JulianDate -> SunDetails
sunDetails jd =
  let t = numberOfCenturies j1900 jd
      epsilon = reduceToZeroRange 360 $ 279.6966778 + 36000.76892*t + 0.0003025*t*t
      omega = reduceToZeroRange 360 $ 281.2208444 + 1.719175*t + 0.000452778*t*t
      e = 0.01675104 - 0.0000418*t - 0.000000126*t*t
  in SunDetails jd (DD epsilon) (DD omega) e
