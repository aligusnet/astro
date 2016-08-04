module Data.Astro.Calendar
(
  Day(..)
  , TimeOfDay(..)
  , LocalTime(..)
  , JulianDayNumber(..)
  , isLeapYear
  , dayNumber
  , easterDayInYear
  , toDecimalHours
  , fromDecimalHours
  , fromDateTime
  , toDateTime
  , dayOfWeek
  , toSiderealTime
  , splitToDayAndTime
)
where

import Data.Time.Calendar (Day(..), fromGregorian, toGregorian)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))

import Data.Astro.Utils (fromFixed, trunc, fraction, reduceToZeroRange)

type DateBaseType = Double


-- | A number of days since noon of 1 January 4713 BC
newtype JulianDayNumber = JulianDayNumber DateBaseType
                     deriving (Show, Eq)


julianStartDateTime = LocalTime (fromGregorian (-4712) 1 1) (TimeOfDay 12 0 0)


instance Num JulianDayNumber where
  (+) (JulianDayNumber d1) (JulianDayNumber d2) = JulianDayNumber (d1+d2)
  (-) (JulianDayNumber d1) (JulianDayNumber d2) = JulianDayNumber (d1-d2)
  (*) (JulianDayNumber d1) (JulianDayNumber d2) = JulianDayNumber (d1*d2)
  negate (JulianDayNumber d) = JulianDayNumber (negate d)
  abs (JulianDayNumber d) = JulianDayNumber (abs d)
  signum (JulianDayNumber d) = JulianDayNumber (signum d)
  fromInteger int = JulianDayNumber (fromInteger int)


-- | Create Julian Day Number from DateTime
fromDateTime :: LocalTime -> JulianDayNumber
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
      e = toDecimalHours time
      jd = fromIntegral (b + c + d + day) + e + 1720994.5  -- add 1720994.5 to process BC/AC border
  in JulianDayNumber jd


toDateTime :: JulianDayNumber -> LocalTime
toDateTime (JulianDayNumber jd) =
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
   in (LocalTime (fromGregorian year month day) (fromDecimalHours f))


-- | Extract Day and Time parts of Date
splitToDayAndTime :: JulianDayNumber -> (JulianDayNumber, JulianDayNumber)
splitToDayAndTime jd@(JulianDayNumber n) =
  let day = JulianDayNumber $ 0.5 + trunc (n - 0.5)
      time = jd - day
  in (day, time)


-- | Get Julian date corresponding to midnight
removeHours :: JulianDayNumber -> JulianDayNumber
removeHours jd =
  let (d, _) = splitToDayAndTime jd
  in d

----------------------------------------------------------------------
-- Sidereal Time


-- | Convert from Universal Time to Greenwich Sidereal Time (GST)
toSiderealTime :: JulianDayNumber -> TimeOfDay
toSiderealTime jd =
  let (JulianDayNumber day, JulianDayNumber time) = splitToDayAndTime jd
      s = day - 2451545.0
      t = s/36525.0
      t' = 6.697374558 + 2400.051336*t + 0.000025862*t*t
      time' = reduceToZeroRange 24 $ t' + time*24*1.002737909
  in fromDecimalHours (time'/24)


------------------------------------------------------------------------


-- | Get Day of the Week
-- 0 is for Sunday, 1 for manday and 6 for Saturday
dayOfWeek :: JulianDayNumber -> Int
dayOfWeek jd =
  let JulianDayNumber d = removeHours jd
      (_, f) = properFraction $ (d+1.5) / 7
  in round (7*f)

------------------------------------------------------
-- Gregorian Calendar

-- | Convert Time to Decimal Hours (fraction of Date)
toDecimalHours :: RealFrac a => TimeOfDay -> a
toDecimalHours (TimeOfDay hours minutes seconds) = (hours' + (minutes' + seconds' / 60) / 60) / 24
  where hours' = fromIntegral hours
        minutes' = fromIntegral minutes
        seconds' = fromFixed seconds


-- | Convert Decimal Hours to Time
fromDecimalHours :: RealFrac a => a -> TimeOfDay
fromDecimalHours n =
  let hours = n*24
      hours' = truncate hours
      minutes = (hours - fromIntegral hours')*60
      minutes' = truncate minutes
      seconds = (minutes - fromIntegral minutes') * 60
      seconds' = realToFrac seconds
  in TimeOfDay hours' minutes' seconds'


-- Date after 15 October 1582 belongs to Gregorian Calendar
-- Before this date - to Julian Calendar
isGregorianDate :: Day -> Bool
isGregorianDate date = date >= moveToGregorianCalendarDate
  where moveToGregorianCalendarDate = fromGregorian 1582 10 15


gregorianDateAdjustment :: Day -> Int
gregorianDateAdjustment date =
  if isGregorianDate date
  then let (year, month, _) = toGregorian date
           y = if month < 3 then year - 1 else year
           y' = fromIntegral y
           a = truncate (y' / 100)
       in 2 - a + truncate(fromIntegral a/4)
  else 0


-- | Check Gregorian calendar leap year
isLeapYear :: Integer -> Bool
isLeapYear year =
  year `mod` 4 == 0
  && (year `mod` 100 /= 0 || year `mod` 400 == 0)


-- | Day Number in a year
dayNumber :: Day -> Int
dayNumber date =
  (daysBeforeMonth year month) + day
  where (year, month, day) = toGregorian date
  

daysBeforeMonth :: Integer -> Int -> Int
daysBeforeMonth year month =
  let a = if isLeapYear year then 62 else 63
      month' = (fromIntegral month) :: Double
  in if month > 2 then
    truncate $ ((month' + 1.0) * 30.6) - a
  else truncate $ (month' - 1.0)*a*0.5


-- | Get Easter date
-- function uses absolutely crazy Butcher's algorithm
easterDayInYear :: Int -> Day
easterDayInYear year =
  let  a = year `mod` 19
       b = year `div` 100
       c = year `mod` 100
       d = b `div` 4
       e = b `mod` 4
       f = (b+8) `div` 25
       g = (b-f+1) `div` 3
       h = (19*a+b-d-g+15) `mod` 30
       i = c `div` 4
       k = c `mod` 4
       l = (32+2*e+2*i-h-k) `mod` 7
       m = (a+11*h+22*l) `div` 451
       n' = (h+l-7*m+114)
       n = n' `div` 31
       p = n' `mod` 31
  in fromGregorian (fromIntegral year) n (p+1)
