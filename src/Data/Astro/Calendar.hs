module Data.Astro.Calendar
(
  Date(..)
  , easterDayInYear
)
where

data Date = Date {
  getYear :: Int
  , getMonth :: Int
  , getDay :: Int
  } deriving (Show, Eq)

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
