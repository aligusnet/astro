{-|
Module: Data.Astro.Moon
Description: Calculation characteristics of the Moon
Copyright: Alexander Ignatyev, 2016

Calculation characteristics of the Moon.

= Example

@
import Data.Astro.Time.JulianDate
import Data.Astro.Coordinate
import Data.Astro.Types
import Data.Astro.Effects
import Data.Astro.CelestialObject.RiseSet
import Data.Astro.Moon

ro :: GeographicCoordinates
ro = GeoC (fromDMS 51 28 40) (-(fromDMS 0 0 5))

dt :: LocalCivilTime
dt = lctFromYMDHMS (DH 1) 2017 6 25 10 29 0

today :: LocalCivilDate
today = lcdFromYMD (DH 1) 2017 6 25

jd :: JulianDate
jd = lctUniversalTime dt

-- distance from the Earth to the Moon in kilometres
mdu :: MoonDistanceUnits
mdu = moonDistance1 j2010MoonDetails jd
-- MDU 0.9550170577020396

distance :: Double
distance = mduToKm mdu
-- 367109.51199772174

-- Angular Size
angularSize :: DecimalDegrees
angularSize = moonAngularSize mdu
-- DD 0.5425033990980761

-- The Moon's coordinates
position :: JulianDate -> EquatorialCoordinates1
position = moonPosition1 j2010MoonDetails

ec1 :: EquatorialCoordinates1
ec1 = position jd
-- EC1 {e1Declination = DD 18.706180658927323, e1RightAscension = DH 7.56710547682055}

hc :: HorizonCoordinates
hc = ec1ToHC ro jd ec1
-- HC {hAltitude = DD 34.57694951316064, hAzimuth = DD 103.91119101451832}

-- Rise and Set
riseSet :: RiseSetMB
riseSet = riseAndSet2 0.000001 position ro verticalShift today
-- RiseSet
--    (Just (2017-06-25 06:22:51.4858 +1.0,DD 57.81458864497365))
--    (Just (2017-06-25 22:28:20.3023 +1.0,DD 300.4168238905249))

-- Phase
phase :: Double
phase = moonPhase j2010MoonDetails jd
-- 2.4716141948212922e-2


sunEC1 :: EquatorialCoordinates1
sunEC1 = sunPosition2 jd
-- EC1 {e1Declination = DD 23.37339098989099, e1RightAscension = DH 6.29262026252748}

limbAngle :: DecimalDegrees
limbAngle = moonBrightLimbPositionAngle ec1 sunEC1
-- DD 287.9869373767473
@
-}

module Data.Astro.Moon
(
  moonPosition1
  , moonPosition2
  , moonDistance1
  , moonAngularSize
  , moonHorizontalParallax
  , moonPhase
  , moonBrightLimbPositionAngle
)

where

import qualified Data.Astro.Utils as U
import Data.Astro.Types (DecimalDegrees(..), GeographicCoordinates, toRadians, fromRadians, kmToAU)
import Data.Astro.Time.JulianDate (JulianDate(..), numberOfDays)
import Data.Astro.Coordinate (EquatorialCoordinates1(..), EclipticCoordinates(..), eclipticToEquatorial)
import Data.Astro.Planet (planetBrightLimbPositionAngle)
import Data.Astro.Sun (sunDetails, sunMeanAnomaly2, sunEclipticLongitude2)
import Data.Astro.Moon.MoonDetails (MoonDetails(..), MoonDistanceUnits(..), j2010MoonDetails, mduToKm)
import Data.Astro.Effects (parallax)


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


-- | Calculate Equatorial Coordinates of the Moon with the given MoonDetails,
-- distance to the Moon, geographic coordinates of the onserver,
-- height above sea-level of the observer measured in metres (20 is a good reasonable value for the height)
-- and at the given JulianDate.
-- It is recommended to use 'j2010MoonDetails' as a first parameter,
-- to obtain the distance to the Moon you can use `moonDistance1` function.
-- `moonPosition2` takes into account parallax effect.
moonPosition2 :: MoonDetails -> MoonDistanceUnits -> GeographicCoordinates -> Double -> JulianDate -> EquatorialCoordinates1
moonPosition2 md distance coords height jd =
  let p = moonPosition1 md jd
  in parallax coords height (kmToAU $ mduToKm distance) jd p


-- | Calculates the Moon's Distance at the given julian date.
-- Returns distance to the Moon
-- moonDistance1 :: JulianDate -> MoonDistanceUnits
-- you can use 'mduToKm' (defined in "Data.Astro.Moon.MoonDetails") to convert result to kilometers
moonDistance1 :: MoonDetails -> JulianDate -> MoonDistanceUnits
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


-- | Calculate the Moon's angular size at the given distance.
moonAngularSize :: MoonDistanceUnits -> DecimalDegrees
moonAngularSize (MDU p) = (mdBigTheta j2010MoonDetails) / (DD p)


-- | Calculates the Moon's horizontal parallax at the given distance.
moonHorizontalParallax :: MoonDistanceUnits -> DecimalDegrees
moonHorizontalParallax (MDU p) = (mdPi j2010MoonDetails) / (DD p)


-- | Calculates the Moon's phase (the area of the visible segment expressed as a fraction of the whole disk)
-- at the given universal time.
moonPhase :: MoonDetails -> JulianDate -> Double
moonPhase md ut =
  let sd = sunDetails ut
      lambdaS = sunEclipticLongitude2 sd
      ms = sunMeanAnomaly2 sd
      mmq = meanMoonQuantities md ut
      MQ ml _ _ = correctedMoonQuantities lambdaS ms mmq
      d = toRadians $ ml - lambdaS
      f = 0.5 * (1 - cos d)
  in f



-- | Calculate the Moon's position-angle of the bright limb.
-- It takes the Moon's coordinates and the Sun's coordinates.
-- Position-angle is the angle of the midpoint of the illuminated limb
-- measured eastwards from the north point of the disk.
moonBrightLimbPositionAngle :: EquatorialCoordinates1 -> EquatorialCoordinates1 -> DecimalDegrees
moonBrightLimbPositionAngle = planetBrightLimbPositionAngle


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
