{-|
Module: Data.Astro.Time.JulianDate
Description: Julian Date
Copyright: Alexander Ignatyev, 2016


Julian date is the continuous count of days since noon on January 1, 4713 BC,
the beginning of the Julian Period.

= Examples

== /JulianDate/
@
import Data.Astro.Time.JulianDate

-- 2017-06-25 9:29:00 (GMT)
jd :: JulianDate
jd = fromYMDHMS 2017 6 25 9 29 0
-- JD 2457929.895138889
@

== /LocalCiviTime and LocalCivilDate/

@
import Data.Astro.Time.JulianDate
import Data.Astro.Types

-- 2017-06-25 10:29:00 +0100 (BST)
lct :: LocalCivilTime
lct = lctFromYMDHMS (DH 1) 2017 6 25 10 29 0
-- 2017-06-25 10:29:00.0000 +1.0

lctJD :: JulianDate
lctJD = lctUniversalTime lct
-- JD 2457929.895138889

lctTZ :: DecimalHours
lctTZ = lctTimeZone lct
-- DH 1.0

lcd :: LocalCivilDate
lcd = lcdFromYMD (DH 1) 2017 6 25

lcdJD :: JulianDate
lcdJD = lcdDate lcd
-- JD 2457929.5

lcdTZ :: DecimalHours
lcdTZ = lcdTimeZone lcd
-- DH 1.0
@
-}

module Data.Astro.Time.JulianDate
(
  JulianDate(..)
  , julianStartDateTime
  , LocalCivilTime(..)
  , LocalCivilDate(..)
  , TimeBaseType
  , numberOfDays
  , numberOfYears
  , numberOfCenturies
  , addHours
  , fromYMD
  , fromYMDHMS
  , toYMDHMS
  , dayOfWeek
  , splitToDayAndTime
  , lctFromYMDHMS
  , lctToYMDHMS
  , lcdFromYMD
  , printLctHs
)

where

import Text.Printf (printf)

import Data.Astro.Types(DecimalHours(..), fromHMS, toHMS)
import Data.Astro.Time.GregorianCalendar (gregorianDateAdjustment)
import Data.Astro.Utils (trunc, fraction)


type TimeBaseType = Double

-- | A number of days since noon of 1 January 4713 BC
newtype JulianDate = JD TimeBaseType
                     deriving (Show, Eq)


-- | Represents Local Civil Time
data LocalCivilTime = LCT {
  lctTimeZone :: DecimalHours   -- Time Zone correction
  , lctUniversalTime :: JulianDate
  } deriving (Eq)


instance Show LocalCivilTime where
  show = printLct


-- | Local Civil Date, used for time conversions when base date is needed
data LocalCivilDate = LCD {
  lcdTimeZone :: DecimalHours
  , lcdDate :: JulianDate
  } deriving (Eq)


-- | Beginning of the Julian Period
julianStartDateTime = fromYMDHMS (-4712) 1 1 12 0 0


instance Num JulianDate where
  (+) (JD d1) (JD d2) = JD (d1+d2)
  (-) (JD d1) (JD d2) = JD (d1-d2)
  (*) (JD d1) (JD d2) = JD (d1*d2)
  negate (JD d) = JD (negate d)
  abs (JD d) = JD (abs d)
  signum (JD d) = JD (signum d)
  fromInteger int = JD (fromInteger int)


-- | Return number of days since the first argument till the second one
numberOfDays :: JulianDate -> JulianDate -> TimeBaseType
numberOfDays (JD jd1) (JD jd2) = jd2 - jd1


-- | Return number of years since the first argument till the second one
numberOfYears :: JulianDate -> JulianDate -> TimeBaseType
numberOfYears (JD jd1) (JD jd2) = (jd2-jd1) / 365.25


-- | Return number of centuries since the first argument till the second one
numberOfCenturies :: JulianDate -> JulianDate -> TimeBaseType
numberOfCenturies (JD jd1) (JD jd2) = (jd2-jd1) / 36525


-- | add Decimal Hours
addHours :: DecimalHours -> JulianDate -> JulianDate
addHours (DH hours) jd = jd + (JD $ hours/24)


-- | Create Julian Date.
-- It takes year, month [1..12], Day [1..31].
fromYMD :: Integer -> Int -> Int -> JulianDate
fromYMD year month day =
  let (y, m) = if month < 3 then (year-1, month+12) else (year, month)
      y' = fromIntegral y
      m' = fromIntegral m
      b = gregorianDateAdjustment year month day
      c = if y < 0
          then truncate (365.25*y' - 0.75)  -- 365.25 - number of solar days in a year
          else truncate (365.25*y')
      d = truncate (30.6001 * (m'+1))
      jd = fromIntegral (b + c + d + day) + 1720994.5  -- add 1720994.5 to process BC/AC border
  in JD jd


-- | Create Julian Date.
-- It takes year, month [1..12], Day [1..31], hours, minutes, seconds.
fromYMDHMS :: Integer -> Int -> Int -> Int -> Int -> TimeBaseType -> JulianDate
fromYMDHMS year month day hs ms ss = addHours (fromHMS hs ms ss) (fromYMD year month day)


-- | It returns year, month [1..12], Day [1..31], hours, minutes, seconds.
toYMDHMS :: JulianDate -> (Integer, Int, Int, Int, Int, TimeBaseType)
toYMDHMS (JD jd) =
  let (i, time) = fraction (jd + 0.5)
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
      (h, m, s) = toHMS $ DH $ 24*time
   in (year, month, day, h, m, s)


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


-- | Create LocalCivilTime from tize zone, local year, local month, local day, local hours, local minutes and local secunds.
lctFromYMDHMS :: DecimalHours ->Integer -> Int -> Int -> Int -> Int -> TimeBaseType -> LocalCivilTime
lctFromYMDHMS tz y m d hs ms ss =
  let jd = fromYMDHMS y m d hs ms ss
      jd' = addHours (-tz) jd
  in LCT tz jd'


-- | Get from LocalCivilTime local year, local month, local day, local hours, local minutes and local secunds.
lctToYMDHMS :: LocalCivilTime -> (Integer, Int, Int, Int, Int, TimeBaseType)
lctToYMDHMS (LCT tz jd)= toYMDHMS (addHours tz jd)


-- Create LocalCivilDate from time zone, local year, local month, local day
lcdFromYMD :: DecimalHours -> Integer -> Int -> Int -> LocalCivilDate
lcdFromYMD tz y m d = LCD tz (fromYMD y m d)


-- | Print Local Civil Time in human-readable format
printLct :: LocalCivilTime -> String
printLct lct =
  printf "%d-%02d-%02d %02d:%02d:%07.4f %+03.1f" y m d hs ms ss tz
  where (y, m, d, hs, ms, ss) = lctToYMDHMS lct
        DH tz = lctTimeZone lct


-- | Print local civil time in machine readable format
printLctHs :: LocalCivilTime -> String
printLctHs lct =
  printf "lctFromYMDHMS (%1.0f) %d %d %d %d %d %.4f" tz y m d hs ms ss
  where (y, m, d, hs, ms, ss) = lctToYMDHMS lct
        DH tz = lctTimeZone lct
