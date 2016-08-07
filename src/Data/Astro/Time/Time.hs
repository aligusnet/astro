module Data.Astro.Time.Time
(
  BaseType(..)
  , toDecimalHours
  , fromDecimalHours
)

where


import Data.Astro.Time (TimeOfDay(..))
import Data.Astro.Utils (fromFixed)

type BaseType = Double

toDecimalHours :: RealFrac a => TimeOfDay -> a
toDecimalHours (TimeOfDay hours minutes seconds) = hours' + (minutes' + seconds' / 60) / 60
  where hours' = fromIntegral hours
        minutes' = fromIntegral minutes
        seconds' = fromFixed seconds


-- | Convert Decimal Hours to Time
fromDecimalHours :: RealFrac a => a -> TimeOfDay
fromDecimalHours hours =
  let hours' = truncate hours
      minutes = (hours - fromIntegral hours')*60
      minutes' = truncate minutes
      seconds = (minutes - fromIntegral minutes') * 60
      seconds' = realToFrac seconds
  in TimeOfDay hours' minutes' seconds'
