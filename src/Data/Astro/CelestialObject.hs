{-|
Module: Data.Astro.CelestialObject
Description: Computations characteristics of selestial objects
Copyright: Alexander Ignatyev, 2016

Computations characteristics of selestial objects.
-}

module Data.Astro.CelestialObject
(
  RiseAndSetTimeAzimuth(..)
  , angleEquatorial
  , angleEcliptic
  , riseAndSet
)

where

import Data.Astro.Types (DecimalDegrees, DecimalHours, toRadians, fromRadians, toDecimalHours, fromDecimalHours)
import Data.Astro.Utils (reduceToZeroRange)
import Data.Astro.Coordinate (EquatorialCoordinates1(..), EclipticCoordinates(..))


-- | LST and Azimuth for Rise and Set of a celestial object
data RiseAndSetTimeAzimuth
  -- | (Local Sidereal Time, Azimuth) for Rise and Set of the celestial object
  = RiseSet (DecimalHours, DecimalDegrees) (DecimalHours, DecimalDegrees)
  -- | The celestial object is always above the horizon
  | Circumpolar
  -- | The celestial object is always below the horizon
  | NeverRises
  deriving (Show, Eq)


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


-- | Calculate rise and set local sidereal time of a celestial object.
-- It takes the equatorial coordinates of the celestial object,
-- vertical shift and the latitude of the observation.
-- To calculate /vertical shift/ for stars use function 'refract' from "Data.Astro.Effects".
-- In most cases you can assume that /vertical shift/ equals 0.566569 (34 arcmins ~ 'refract (DD 0) 12 1012').
riseAndSet :: EquatorialCoordinates1 -> DecimalDegrees -> DecimalDegrees -> RiseAndSetTimeAzimuth
riseAndSet (EC1 delta alpha) shift lat =
  let delta' = toRadians delta
      shift' = toRadians shift
      lat' = toRadians lat
      cosH = cosOfHourAngle delta' shift' lat'
  in sortRiseSet cosH delta' shift' lat'

  where sortRiseSet :: Double -> Double -> Double -> Double -> RiseAndSetTimeAzimuth
        sortRiseSet cosH delta shift latitude
          | cosH < -1 = Circumpolar
          | cosH > 1 = NeverRises
          | otherwise = calcTimesAndAzimuths alpha (toHours $ acos cosH) delta shift latitude

        toHours :: Double -> DecimalHours
        toHours = toDecimalHours . fromRadians

        cosOfHourAngle :: Double -> Double -> Double -> Double
        cosOfHourAngle delta shift latitude = -((sin shift) + (sin latitude)*(sin delta)) / ((cos latitude)*(cos delta))

        calcTimesAndAzimuths :: DecimalHours -> DecimalHours -> Double -> Double -> Double -> RiseAndSetTimeAzimuth
        calcTimesAndAzimuths alpha hourAngle delta shift latitude =
          let lstRise = reduceToZeroRange 24 $ alpha - hourAngle
              lstSet = reduceToZeroRange 24 $ alpha + hourAngle
              azimuthRise = reduceToZeroRange (2*pi) $ acos $ ((sin delta) + (sin shift)*(sin latitude)) / ((cos shift)*(cos latitude))
              azimuthSet = 2*pi - azimuthRise
          in RiseSet (lstRise, fromRadians azimuthRise) (lstSet, fromRadians azimuthSet)
