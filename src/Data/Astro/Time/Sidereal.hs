{-
Module: Data.Astro.Time.Sideral
Description: Sideral Time
Copyright: Alexander Ignatyev, 2016

According to the Sidereal Clock any observed star returns to the same position
in the sky every 24 hours.

Each sidereal day is shorter than the solar day, 24 hours of sidereal time
corresponding to 23:56:04.0916 of solar time.
-}
module Data.Astro.Time.Sidereal
(
  toSiderealTime
  , fromSiderealTime
)
where

import Data.Astro.Time (BaseType, TimeOfDay(..), toDecimalHours)
import Data.Astro.Time.JulianDate (JulianDate(..), splitToDayAndTime)
import Data.Astro.Utils (reduceToZeroRange)

-- | Convert from Universal Time (UT) to Greenwich Sidereal Time (GST)
toSiderealTime :: JulianDate -> JulianDate
toSiderealTime jd =
  let (JD day, JD time) = splitToDayAndTime jd
      t = solarSiderealTimesDiff day
      time' = reduceToZeroRange 24 $ time*24/siderealDayLength + t
  in JD $ day + time'/24

-- | Convert from Greenwich Sidereal Time (GST) to Universal Time (UT)
-- because the sidereal day is shorter than the solar day (see comment to the module).
-- In case of such ambiguity the early time will be returned.
-- You can easily check the ambiguity: if time is equal or less 00:03:56
-- you can get the second time by adding 23:56:04
fromSiderealTime :: JulianDate -> JulianDate
fromSiderealTime jd =
  let (JD day, JD time) = splitToDayAndTime jd
      t = solarSiderealTimesDiff day
      time' = (reduceToZeroRange 24 (time*24-t)) * siderealDayLength
  in JD $ day + time'/24


-- Sidereal time internal functions

-- sidereal 24h correspond to 23:56:04 of solar time
siderealDayLength :: BaseType
siderealDayLength = toDecimalHours (TimeOfDay 23 56 04.0916)


solarSiderealTimesDiff :: BaseType -> BaseType
solarSiderealTimesDiff d =
  let t = (d - 2451545.0)/36525.0
  in 6.697374558 + 2400.051336*t + 0.000025862*t*t
