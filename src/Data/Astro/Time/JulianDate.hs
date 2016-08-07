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
  , fromDateTime
  , toDateTime
  , dayOfWeek
  , splitToDayAndTime

)

where

import Data.Time.Calendar (fromGregorian, toGregorian)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))

import Data.Astro.Time (BaseType, toDecimalHours, fromDecimalHours)
import Data.Astro.Time.GregorianCalendar (gregorianDateAdjustment)
import Data.Astro.Utils (trunc, fraction)


-- | A number of days since noon of 1 January 4713 BC
newtype JulianDate = JD BaseType
                     deriving (Show, Eq)


-- | Beginning of the Julian Period
julianStartDateTime = LocalTime (fromGregorian (-4712) 1 1) (TimeOfDay 12 0 0)


instance Num JulianDate where
  (+) (JD d1) (JD d2) = JD (d1+d2)
  (-) (JD d1) (JD d2) = JD (d1-d2)
  (*) (JD d1) (JD d2) = JD (d1*d2)
  negate (JD d) = JD (negate d)
  abs (JD d) = JD (abs d)
  signum (JD d) = JD (signum d)
  fromInteger int = JD (fromInteger int)


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
      e = (toDecimalHours time) / 24
      jd = fromIntegral (b + c + d + day) + e + 1720994.5  -- add 1720994.5 to process BC/AC border
  in JD jd


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
   in (LocalTime (fromGregorian year month day) (fromDecimalHours $ f*24))



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
