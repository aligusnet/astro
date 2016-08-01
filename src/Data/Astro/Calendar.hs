module Data.Astro.Calendar
(
  Date(..)
  , isLeapYear
  , dayNumber
  , easterDayInYear
)
where

data Date = Date {
  getYear :: Int
  , getMonth :: Int
  , getDay :: Int
  } deriving (Show, Eq)


-- | Check Gregorian calendar leap year
isLeapYear :: Int -> Bool
isLeapYear year =
  year `mod` 4 == 0
  && (year `mod` 100 /= 0 || year `mod` 400 == 0)


-- | Day Number in a year
dayNumber :: Date -> Int
dayNumber (Date year month day) =
  (daysBeforeMonth year month) + day
  

daysBeforeMonth :: Int -> Int -> Int
daysBeforeMonth year month =
  let a = if isLeapYear year then 62 else 63
      month' = (fromIntegral month) :: Double
  in if month > 2 then
    truncate $ ((month' + 1.0) * 30.6) - a
  else truncate $ (month' - 1.0)*a*0.5

-- | Get Easter date
-- function uses absolutely crazy Butcher's algorithm
easterDayInYear :: Int -> Date
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
  in Date year n (p+1)
