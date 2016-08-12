{-|
Module: Data.Astro.Time.JulianDate
Description: Julian Date
Copyright: Alexander Ignatyev, 2016


Julian date is the continuous count of days since noon on January 1, 4713 BC,
the beginning of the Julian Period.
-}

module Data.Astro.Time.JulianDate
(
  JulianDate(..)
  , TimeBaseType
    -- * Epochs
    -- ** Besselian Epochs
  , b1900
  , b1950
    -- ** New Epochs
  , j1900
  , j2000
  , j2050
    -- *
  , numberOfYears
  , numberOfCenturies
  , fromDateTime
  , toDateTime
  , dayOfWeek
  , splitToDayAndTime
  , utToLCT
  , lctToUT
)

where

import Data.Astro.Time.Types(LocalTime(..), TimeOfDay(..), fromGregorian, toGregorian)
import Data.Astro.Time.GregorianCalendar (gregorianDateAdjustment)
import Data.Astro.Utils (trunc, fraction, fromFixed)


type TimeBaseType = Double

-- | A number of days since noon of 1 January 4713 BC
newtype JulianDate = JD TimeBaseType
                     deriving (Show, Eq)


-- | Beginning of the Julian Period
julianStartDateTime = LocalTime (fromGregorian (-4712) 1 1) (TimeOfDay 12 0 0)


-- | Epoch B1900.0, 1900 January 0.8135
b1900 :: JulianDate
b1900 = JD 2415020.3135

-- | Epoch B1950.0, January 0.9235
b1950 :: JulianDate
b1950 = JD 2433282.4235


-- | Epoch J1900.0 1900 January 0.5
j1900 :: JulianDate
j1900 = JD 2415020.0

-- | Epoch J2000.0, 12h on 1 January 2000
j2000 :: JulianDate
j2000 = JD 2451545.0

-- | Epoch J2050.0, 12h on 1 January 2000
j2050 :: JulianDate
j2050 = JD 2469807.50


instance Num JulianDate where
  (+) (JD d1) (JD d2) = JD (d1+d2)
  (-) (JD d1) (JD d2) = JD (d1-d2)
  (*) (JD d1) (JD d2) = JD (d1*d2)
  negate (JD d) = JD (negate d)
  abs (JD d) = JD (abs d)
  signum (JD d) = JD (signum d)
  fromInteger int = JD (fromInteger int)


-- | Return number of years since the first argument till the second one
numberOfYears :: JulianDate -> JulianDate -> TimeBaseType
numberOfYears (JD jd1) (JD jd2) = (jd2-jd1) / 365.25


-- | Return number of centuries since the first argument till the second one
numberOfCenturies :: JulianDate -> JulianDate -> TimeBaseType
numberOfCenturies (JD jd1) (JD jd2) = (jd2-jd1) / 36525


-- | Create Julian Date from DateTime
fromDateTime :: LocalTime -> JulianDate
fromDateTime (LocalTime date time) =
  let (year, month, day) = toGregorian date
      (y, m) = if month < 3 then (year-1, month+12) else (year, month)
      y' = fromIntegral y
      m' = fromIntegral m
      b = gregorianDateAdjustment date
      c = if y < 0
          then truncate (365.25*y' - 0.75)  -- 365.25 - number of solar days in a year
          else truncate (365.25*y')
      d = truncate (30.6001 * (m'+1))
      e = toDecimalDays time
      jd = fromIntegral (b + c + d + day) + e + 1720994.5  -- add 1720994.5 to process BC/AC border
  in JD jd


-- | Comvert Julian Date to DateTime
toDateTime :: JulianDate -> LocalTime
toDateTime (JD jd) =
  let (i, f) = fraction (jd + 0.5)
      b = if i > 2299160  -- 2299161 - first day of Georgian Calendar
          then let a = trunc $ (i-1867216.25)/36524.25
               in i + a - trunc (a*0.25) + 1
          else i
      c = b + 1524
      d = trunc $ (c-122.1)/365.25
      e = trunc $ d * 365.25
      g = trunc $ (c-e)/30.6001
      day = truncate $ c - e - trunc (30.6001*g)
      month = truncate $ if g < 13.5 then g - 1 else g - 13
      year = truncate $ if month > 2 then d-4716 else d-4715
   in (LocalTime (fromGregorian year month day) (fromDecimalDays f))



-- | Get Day of the Week
-- 0 is for Sunday, 1 for manday and 6 for Saturday
dayOfWeek :: JulianDate -> Int
dayOfWeek jd =
  let JD d = removeHours jd
      (_, f) = properFraction $ (d+1.5) / 7
  in round (7*f)


-- | Extract Day and Time parts of Date
splitToDayAndTime :: JulianDate -> (JulianDate, JulianDate)
splitToDayAndTime jd@(JD n) =
  let day = JD $ 0.5 + trunc (n - 0.5)
      time = jd - day
  in (day, time)


-- | Get Julian date corresponding to midnight
removeHours :: JulianDate -> JulianDate
removeHours jd =
  let (d, _) = splitToDayAndTime jd
  in d


-- | Convert Local Civil Time (LCT) to Universal Time (UT)
-- The function takes time zone offset in hours and julian date
lctToUT :: Double -> JulianDate -> JulianDate
lctToUT offset (JD jd) = JD $ jd - (offset/24.0)


-- | Convert Universal Time (UT) to Local Civil Time (LCT)
-- The function takes time zone offset in hours and julian date
utToLCT :: Double -> JulianDate -> JulianDate
utToLCT offset (JD jd) = JD $ jd + (offset/24)



toDecimalDays :: RealFrac a => TimeOfDay -> a
toDecimalDays (TimeOfDay hours minutes seconds) = (hours' + (minutes' + seconds' / 60) / 60)/24
  where hours' = fromIntegral hours
        minutes' = fromIntegral minutes
        seconds' = fromFixed seconds


fromDecimalDays :: RealFrac a => a -> TimeOfDay
fromDecimalDays days =
  let hours = 24*days
      hours' = truncate hours
      minutes = (hours - fromIntegral hours')*60
      minutes' = truncate minutes
      seconds = (minutes - fromIntegral minutes') * 60
      seconds' = realToFrac seconds
  in TimeOfDay hours' minutes' seconds'
