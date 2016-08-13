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
  , RiseSet(..)
  , SunRiseSet(..)
  , j2010
  , sunDetails
  , j2010SunDetails
  , sunEclipticLongitude1
  , sunEclipticLongitude2
  , sunPosition1
  , sunPosition2
  , sunDistance
  , sunAngularSize
  , sunRiseAndSet
  , equationOfTime
  , solarElongation
)

where

import qualified Data.Astro.Utils as U
import Data.Astro.Types (DecimalDegrees(..), DecimalHours(..)
                        , toDecimalHours, fromDecimalHours
                        , toRadians, fromRadians
                        , GeographicCoordinates(..))
import Data.Astro.Time.JulianDate (JulianDate(..), j1900, numberOfCenturies, splitToDayAndTime, addHours)
import Data.Astro.Time.Sidereal (gstToUT)
import Data.Astro.Coordinate (EquatorialCoordinates1(..), EclipticCoordinates(..), eclipticToEquatorial)
import Data.Astro.Effects.Nutation (nutationLongitude)
import Data.Astro.CelestialObject (RiseSet(..), RiseSetJD(..), RSInfo(..), riseAndSet, toRiseSetLCT)

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


-- | Calculate the ecliptic longitude of the Sun with the given SunDetails at the given JulianDate
sunEclipticLongitude1 :: SunDetails -> JulianDate -> DecimalDegrees
sunEclipticLongitude1 sd@(SunDetails epoch (DD eps) (DD omega) e) jd =
  let JD d = jd - epoch  -- number of days
      n = reduceTo360 $ (360/tropicalYearLen) * d
      meanAnomaly = reduceTo360 $ n + eps - omega
      ec = (360/pi)*e*(sin $ U.toRadians meanAnomaly)
      DD nutation = nutationLongitude jd
  in DD $ reduceTo360 $ n + ec + eps + nutation


-- | Calculate Equatorial Coordinates of the Sun with the given SunDetails at the given JulianDate.
-- It is recommended to use 'j2010SunDetails' as a first parameter.
sunPosition1 :: SunDetails -> JulianDate -> EquatorialCoordinates1
sunPosition1 sd jd =
  let lambda = sunEclipticLongitude1 sd jd
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


-- | Calculate the ecliptic longitude of the Sun at the given JulianDate
sunEclipticLongitude2 :: JulianDate -> DecimalDegrees
sunEclipticLongitude2 jd =
  let sd = sunDetails jd
      DD omega = sdOmega sd
      DD nu = trueAnomaly2 sd
      DD nutation = nutationLongitude jd
  in DD $ reduceTo360 $ nu + omega + nutation


-- | More accurate method to calculate position of the Sun
sunPosition2 :: JulianDate -> EquatorialCoordinates1
sunPosition2 jd =
  let lambda = sunEclipticLongitude2 jd
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


-- | The Sun's Rise And Set Information (JulianDate and Azimuth)
type SunRiseSet = RiseSet (Maybe (RSInfo JulianDate))


data SetOrRise = SOR (RSInfo JulianDate)
               | SCircumpolar
               | SNeverRises
               deriving (Eq)


-- | Calculatesthe Sun's rise and set
-- It takes coordinates of the observer,
-- time zone ( 0 for UT),
-- vertical shift (good value is 0.833333), date.
-- It returns Nothing if fails to calculate rise and/or set.
-- It should be accurate to within a minute of time.
sunRiseAndSet :: GeographicCoordinates
                 -> Double
                 -> DecimalDegrees
                 -> JulianDate
                 -> SunRiseSet
sunRiseAndSet geoc timeZone shift jd =
  let (day, _) = splitToDayAndTime jd
      offset = (toDecimalHours $ geoLongitude geoc) * 0.5
      sunPosMorining = sunPos day offset
      sunPosEvening = sunPos day (3*offset)
      rise = sunrise (riseSet timeZone) $ riseSet 0 sunPosMorining
      set = sunset (riseSet timeZone)  $ riseSet 0 sunPosEvening
  in fromSORpair rise set

  -- helper functions
  where riseSet = riseAndSetJD geoc shift jd
        sunPos day offset = sunPosition1 j2010SunDetails $ addHours offset day
        fromSORpair (SOR rise) (SOR set) = RiseSet (Just rise) (Just set)
        fromSORpair (SOR rise) _ = RiseSet (Just rise) Nothing
        fromSORpair _ (SOR set) = RiseSet Nothing (Just set)
        fromSORpair SCircumpolar SCircumpolar = Circumpolar
        fromSORpair SNeverRises SNeverRises = NeverRises
        fromSORpair _ _ = RiseSet Nothing Nothing


-- | Approximate method to calculate the Sun's rise.
sunrise :: (EquatorialCoordinates1 -> RiseSetJD)
           -> RiseSetJD
           -> SetOrRise
sunrise riseSet (RiseSet (rise, _) _) =
  let sunPos = sunPosition1 j2010SunDetails rise
  in case riseSet sunPos of
    RiseSet rise' _ -> SOR rise'
    Circumpolar -> SCircumpolar
    NeverRises -> SNeverRises
sunrise _ Circumpolar = SCircumpolar
sunrise _ NeverRises = SNeverRises


-- | Approximate method to calculate the Sun's set.
sunset :: (EquatorialCoordinates1 -> RiseSetJD)
           -> RiseSetJD
           -> SetOrRise
sunset riseSet (RiseSet _ (set, _)) =
  let sunPos = sunPosition1 j2010SunDetails set
  in case riseSet sunPos of
    RiseSet _ set' -> SOR set'
    Circumpolar -> SCircumpolar
    NeverRises -> SNeverRises
sunset _ Circumpolar = SCircumpolar
sunset _ NeverRises = SNeverRises


-- | Calculates set and rise of the celestial object
riseAndSetJD :: GeographicCoordinates
                -> DecimalDegrees
                -> JulianDate
                -> Double
                -> EquatorialCoordinates1
                -> RiseSetJD
riseAndSetJD (GeoC latitude longitude) shift jd timeZone ec
  = toRiseSetLCT longitude timeZone jd $ riseAndSet ec shift latitude


-- | Calculates discrepancy between the mean solar time and real solar time
-- at the given date.
equationOfTime :: JulianDate -> DecimalHours
equationOfTime jd =
  let (day, _) = splitToDayAndTime jd
      midday = addHours (DH 12) day  -- mean solar time
      EC1 _ ra = sunPosition1 j2010SunDetails midday
      ut = gstToUT $ addHours ra day
      JD time = midday - ut
  in DH $ time*24


-- | Calculates the angle between the lines of sight to the Sun and to a celestial object
-- specified by the given coordinates at the given Universal Time.
solarElongation :: EquatorialCoordinates1 -> JulianDate -> DecimalDegrees
solarElongation (EC1 deltaP alphaP) jd =
  let (EC1 deltaS alphaS) = sunPosition1 j2010SunDetails jd
      deltaP' = toRadians deltaP
      alphaP' = toRadians $ fromDecimalHours alphaP
      deltaS' = toRadians deltaS
      alphaS' = toRadians $ fromDecimalHours alphaS
      eps = acos $ (sin deltaP')*(sin deltaS') + (cos $ alphaP' - alphaS')*(cos deltaP')*(cos deltaS')
  in fromRadians eps

