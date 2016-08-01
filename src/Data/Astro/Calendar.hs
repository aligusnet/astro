module Data.Astro.Calendar
(
  Day(..)
  , TimeOfDay(..)
  , LocalTime(..)
  , JulianDayNumber(..)
  , isLeapYear
  , dayNumber
  , easterDayInYear
  , fromTime
  , fromDateTime
)
where

import Data.Time.Calendar (Day(..), fromGregorian, toGregorian)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))

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
          then truncate (365.25*y' - 0.75)
          else truncate (365.25*y')
      d = truncate (30.6001 * (m'+1))
      e = fromTime time
      jd = fromIntegral (b + c + d + day) + e + 1720994.5
  in JulianDayNumber jd


------------------------------------------------------
-- Gregorian Calendar

-- convert Time to a fraction of Date
fromTime :: TimeOfDay -> DateBaseType
fromTime (TimeOfDay hours minutes _) = (hours' + (minutes') / 60) / 24
  where hours' = fromIntegral hours
        minutes' = fromIntegral minutes


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
