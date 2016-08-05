module Data.Astro.Time
(
  Day(..)
  , TimeOfDay(..)
  , LocalTime(..)
  , BaseType(..)
  , toDecimalHours
  , fromDecimalHours
)
where

import Data.Time.Calendar (Day(..))
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))

import Data.Astro.Utils (fromFixed)

type BaseType = Double

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


------------------------------------------------------
-- Gregorian Calendar

