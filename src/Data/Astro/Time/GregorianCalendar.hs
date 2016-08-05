{-
Module: Data.Astro.Time.GregorianCalendar
Description: Gregorian Calendar
Copyright: Alexander Ignatyev, 2016


Gregorian Calendar was introduced by Pope Gregory XIII.
He abolished the days 1582-10-05 to 1582-10-14 inclusive to bring back civil and tropical years back to line.
-}
module Data.Astro.Time.GregorianCalendar
(
  isLeapYear
  , dayNumber
  , easterDayInYear
  , gregorianDateAdjustment
)

where

import Data.Time.Calendar (Day(..), fromGregorian, toGregorian)

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


daysBeforeMonth :: Integer -> Int -> Int
daysBeforeMonth year month =
  let a = if isLeapYear year then 62 else 63
      month' = (fromIntegral month) :: Double
  in if month > 2 then
    truncate $ ((month' + 1.0) * 30.6) - a
  else truncate $ (month' - 1.0)*a*0.5
