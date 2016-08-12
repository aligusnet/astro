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
  , j2010SunDetails
  , sunPosition1
  , sunPosition2
  , sunDistance
  , sunAngularSize
)

where

import qualified Data.Astro.Utils as U
import Data.Astro.Types (DecimalDegrees(..), toRadians, fromRadians)
import Data.Astro.Time.JulianDate (JulianDate(..), j1900, numberOfCenturies)
import Data.Astro.Coordinate (EquatorialCoordinates1(..), EclipticCoordinates(..), eclipticToEquatorial)
import Data.Astro.Effects.Nutation (nutationLongitude)

import Data.Astro.Sun.SunInternals (solveKeplerEquation)


-- | Details of the Sun's apparent orbit at the given epoch
data SunDetails = SunDetails {
  sdEpoch :: JulianDate             -- ^ Epoch
  , sdEpsilon :: DecimalDegrees     -- ^ Ecliptic longitude at the Epoch
  , sdOmega :: DecimalDegrees       -- ^ Ecliptic longitude of perigee at the Epoch
  , sdE :: Double                   -- ^ Eccentricity oforbit at the Epoch
  } deriving (Show)


-- | The Sun's reference Epoch J2010.0 (2010 January 0.0)
j2010 :: JulianDate
j2010 = JD 2455196.5


-- | SunDetails at the Sun's reference Epoch J2010.0
j2010SunDetails :: SunDetails
j2010SunDetails = SunDetails j2010 (DD 279.557208) (DD 283.112438) 0.016705


-- | Semi-major axis
r0 :: Double
r0 = 1.495985e8


-- | Angular diameter at r = r0
theta0 :: DecimalDegrees
theta0 = DD 0.533128


-- | Reduce the value to the range [0, 360)
reduceTo360 :: Double -> Double
reduceTo360 = U.reduceToZeroRange 360


-- | Calculate SunDetails for the given JulianDate.
sunDetails :: JulianDate -> SunDetails
sunDetails jd =
  let t = numberOfCenturies j1900 jd
      epsilon = reduceTo360 $ 279.6966778 + 36000.76892*t + 0.0003025*t*t
      omega = reduceTo360 $ 281.2208444 + 1.719175*t + 0.000452778*t*t
      e = 0.01675104 - 0.0000418*t - 0.000000126*t*t
  in SunDetails jd (DD epsilon) (DD omega) e



-- | Length of a tropical year in days
tropicalYearLen :: Double
tropicalYearLen = 365.242191


-- | Calculate the longitude of the Sun with the given SunDetails at the given JulianDate
longitude :: SunDetails -> JulianDate -> DecimalDegrees
longitude sd@(SunDetails epoch (DD eps) (DD omega) e) jd =
  let JD d = jd - epoch  -- number of days
      n = reduceTo360 $ (360/tropicalYearLen) * d
      meanAnomaly = reduceTo360 $ n + eps - omega
      ec = (360/pi)*e*(sin $ U.toRadians meanAnomaly)
      DD nutation = nutationLongitude jd
  in DD $ reduceTo360 $ n + ec + eps + nutation


-- | Calculate Equatorial Coordinates of the Sun with the given SunDetails at the given JulianDate.
-- It is recommended to used 'j2010SunDetails' as a first parameter.
sunPosition1 :: SunDetails -> JulianDate -> EquatorialCoordinates1
sunPosition1 sd jd =
  let lambda = longitude sd jd
      beta = DD 0
  in eclipticToEquatorial (EcC beta lambda) jd


-- | Calculate true anomaly using the second 'more accurate' method
trueAnomaly2 :: SunDetails -> DecimalDegrees
trueAnomaly2 (SunDetails _ (DD eps) (DD omega) e) =
  let m = U.toRadians $ eps - omega
      bigE = solveKeplerEquation e m 0.000000001
      tanHalfNu = sqrt((1+e)/(1-e)) * tan (0.5 * bigE)
      nu = reduceTo360 $ U.fromRadians $ 2 * (atan tanHalfNu)
  in DD nu


-- | More accurate method to calculate position of the Sun
sunPosition2 :: JulianDate -> EquatorialCoordinates1
sunPosition2 jd =
  let sd = sunDetails jd
      DD omega = sdOmega sd
      DD nu = trueAnomaly2 sd
      DD nutation = nutationLongitude jd
      lambda = DD $ reduceTo360 $ nu + omega + nutation
      beta = DD 0
  in eclipticToEquatorial (EcC beta lambda) jd


-- Distance and Angular Size helper function
dasf sd =
  let e = sdE sd
      nu = toRadians $ trueAnomaly2 sd
  in (1 + e*(cos nu)) / (1 - e*e)


-- | Calculate Sun-Earth distance.
sunDistance :: JulianDate -> Double
sunDistance jd = r0 / (dasf $ sunDetails jd)


-- | Calculate the Sun's angular size (i.e. its angular diameter).
sunAngularSize :: JulianDate -> DecimalDegrees
sunAngularSize jd = theta0 * (DD $ dasf $ sunDetails jd)
