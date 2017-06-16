{-|
Module: Data.Astro.CelestialObject.RiseSet
Description: Computations rise and set of selestial objects
Copyright: Alexander Ignatyev, 2016

Computations rise and set of selestial objects.
-}

module Data.Astro.CelestialObject.RiseSet
(
  RiseSet(..)
  , RSInfo(..)
  , RiseSetLST(..)
  , RiseSetLCT(..)
  , RiseSetMB(..)
  , riseAndSet
  , riseAndSet2
  , toRiseSetLCT
)

where

import Data.Astro.Types (DecimalDegrees, DecimalHours(..)
                        , GeographicCoordinates(..)
                        , toRadians, fromRadians
                        , toDecimalHours)
import Data.Astro.Utils (reduceToZeroRange)
import Data.Astro.Time (lstToLCT)
import Data.Astro.Time.JulianDate (JulianDate(..), LocalCivilTime(..), LocalCivilDate(..), addHours)
import Data.Astro.Time.Sidereal (LocalSiderealTime, dhToLST)
import Data.Astro.Coordinate (EquatorialCoordinates1(..))

-- | Some Info of Rise and Set of a celestial object
data RiseSet a
  -- | Some Info of Rise and Set of the celestial object
  = RiseSet a a
  -- | The celestial object is always above the horizon
  | Circumpolar
  -- | The celestial object is always below the horizon
  | NeverRises
  deriving (Show, Eq)


-- | Rise or Set time and azimuth
type RSInfo a = (a, DecimalDegrees)


-- | LST (Local Sidereal Time) and Azimuth of Rise and Set
type RiseSetLST = RiseSet (RSInfo LocalSiderealTime)


-- | Local Civil Time and Azimuth of Rise and Set
type RiseSetLCT = RiseSet (RSInfo LocalCivilTime)


-- | The optional Rise And optinal Set Information (LocalCivilTime and Azimuth)
type RiseSetMB = RiseSet (Maybe (RSInfo LocalCivilTime))


-- | Calculate rise and set local sidereal time of a celestial object.
-- It takes the equatorial coordinates of the celestial object,
-- vertical shift and the latitude of the observation.
-- To calculate /vertical shift/ for stars use function 'refract' from "Data.Astro.Effects".
-- In most cases you can assume that /vertical shift/ equals 0.566569 (34 arcmins ~ 'refract (DD 0) 12 1012').
riseAndSet :: EquatorialCoordinates1 -> DecimalDegrees -> DecimalDegrees -> RiseSetLST
riseAndSet (EC1 delta alpha) shift lat =
  let delta' = toRadians delta
      shift' = toRadians shift
      lat' = toRadians lat
      cosH = cosOfHourAngle delta' shift' lat'
  in sortRiseSet cosH delta' shift' lat'

  where sortRiseSet :: Double -> Double -> Double -> Double -> RiseSetLST
        sortRiseSet cosH delta shift latitude
          | cosH < -1 = Circumpolar
          | cosH > 1 = NeverRises
          | otherwise = calcTimesAndAzimuths alpha (toHours $ acos cosH) delta shift latitude

        toHours :: Double -> DecimalHours
        toHours = toDecimalHours . fromRadians

        cosOfHourAngle :: Double -> Double -> Double -> Double
        cosOfHourAngle delta shift latitude = -((sin shift) + (sin latitude)*(sin delta)) / ((cos latitude)*(cos delta))

        calcTimesAndAzimuths :: DecimalHours -> DecimalHours -> Double -> Double -> Double -> RiseSetLST
        calcTimesAndAzimuths alpha hourAngle delta shift latitude =
          let lstRise = dhToLST $ reduceToZeroRange 24 $ alpha - hourAngle
              lstSet = dhToLST $ reduceToZeroRange 24 $ alpha + hourAngle
              azimuthRise = reduceToZeroRange (2*pi) $ acos $ ((sin delta) + (sin shift)*(sin latitude)) / ((cos shift)*(cos latitude))
              azimuthSet = 2*pi - azimuthRise
          in RiseSet (lstRise, fromRadians azimuthRise) (lstSet, fromRadians azimuthSet)


-- | Calculate rise and set local sidereal time of a celestial object
-- that changes its equatorial coordinates during the day (the Sun, the Moon, planets).
-- It takes epsilon, the function that returns equatorial coordinates of the celestial object for a given julian date,
-- vertical shift and the latitude of the observation.
-- To calculate /vertical shift/ for stars use function 'refract' from "Data.Astro.Effects".
-- In most cases you can assume that /vertical shift/ equals 0.566569 (34 arcmins ~ 'refract (DD 0) 12 1012').
riseAndSet2 :: DecimalHours
               -> (JulianDate -> EquatorialCoordinates1)
               -> GeographicCoordinates
               -> DecimalDegrees
               -> LocalCivilDate
               -> RiseSetMB
riseAndSet2 eps getPosition geoc shift lcd =
  let day = lcdDate lcd
      pos = getPosition (addHours 12 day)
      rs = riseAndSetLCT geoc lcd shift pos
      rise = calc getRiseTime (getRiseTime rs) 0
      set = calc getSetTime (getSetTime rs) 0
  in case rs of
    Circumpolar -> Circumpolar
    NeverRises -> NeverRises
    _ -> buildResult rise set

  where calc :: (RiseSetLCT -> RSInfo LocalCivilTime) -> RSInfo LocalCivilTime -> Int -> RiseSetLCT
        calc getRSInfo rsi@(time, _) iterNo =
          let pos = getPosition $ lctUniversalTime time
              rs = riseAndSetLCT geoc lcd shift pos
              rsi' = getRSInfo rs
          in case rs of
            Circumpolar -> Circumpolar
            NeverRises -> NeverRises
            _ -> if isOK rsi rsi' || iterNo >= maxIters
                 then rs
                 else calc getRSInfo rsi' (iterNo+1)

        isOK :: RSInfo LocalCivilTime -> RSInfo LocalCivilTime -> Bool
        isOK (t1, _) (t2, _) = (abs d) < (h/24)
          where JD d = (lctUniversalTime t1) - (lctUniversalTime t2)
                DH h = eps

        maxIters = 10

        getRiseTime :: RiseSetLCT -> RSInfo LocalCivilTime
        getRiseTime (RiseSet r _) = r

        getSetTime :: RiseSetLCT -> RSInfo LocalCivilTime
        getSetTime (RiseSet _ s) = s

        buildResult (RiseSet r _) (RiseSet _ s) = RiseSet (Just r) (Just s)
        buildResult (RiseSet r _) _ = RiseSet (Just r) Nothing
        buildResult _ (RiseSet _ s) = RiseSet Nothing (Just s)
        


-- | Calculates set and rise of the celestial object
riseAndSetLCT :: GeographicCoordinates
                -> LocalCivilDate
                -> DecimalDegrees
                -> EquatorialCoordinates1
                -> RiseSetLCT
riseAndSetLCT (GeoC latitude longitude) lcd shift ec
  = toRiseSetLCT longitude lcd $ riseAndSet ec shift latitude


-- | Converts Rise and Set in Local Sidereal Time to Rise and Set in Local Civil Time.
toRiseSetLCT :: DecimalDegrees
               -> LocalCivilDate
               -> RiseSetLST
               -> RiseSetLCT
toRiseSetLCT longitude lcd (RiseSet (rise, azRise) (set, azSet)) =
  let toLCT lst = lstToLCT longitude lcd lst
      rise' = toLCT rise
      set' = toLCT set
  in RiseSet (rise', azRise) (set', azSet)
toRiseSetLCT _ _ Circumpolar  = Circumpolar
toRiseSetLCT _ _ NeverRises = NeverRises


-- | Convert LST in decimal hours to the JuliadDate
-- the second parameter must be desired day at midnignt.
dhToJD :: DecimalHours -> JulianDate -> JulianDate
dhToJD (DH hours) day = day + (JD $ hours/24)
