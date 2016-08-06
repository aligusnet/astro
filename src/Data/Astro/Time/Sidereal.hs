{-|
Module: Data.Astro.Time.Sidereal
Description: Sidereal Time
Copyright: Alexander Ignatyev, 2016

According to the Sidereal Clock any observed star returns to the same position
in the sky every 24 hours.

Each sidereal day is shorter than the solar day, 24 hours of sidereal time
corresponding to 23:56:04.0916 of solar time.
-}

module Data.Astro.Time.Sidereal
(
  utToGST
  , gstToUT
  , gstToLST
  , lstToGST
)
where

import Data.Astro.Time (BaseType, TimeOfDay(..), toDecimalHours)
import Data.Astro.Time.JulianDate (JulianDate(..), splitToDayAndTime)
import Data.Astro.Utils (reduceToZeroRange)
import qualified Data.Astro.Coordinate as C

-- | Convert from Universal Time (UT) to Greenwich Sidereal Time (GST)
utToGST :: JulianDate -> JulianDate
utToGST jd =
  let (JD day, JD time) = splitToDayAndTime jd
      t = solarSiderealTimesDiff day
      time' = reduceToZeroRange 24 $ time*24/siderealDayLength + t
  in JD $ day + time'/24


-- | Convert from Greenwich Sidereal Time (GST) to Universal Time (UT)
-- because the sidereal day is shorter than the solar day (see comment to the module).
-- In case of such ambiguity the early time will be returned.
-- You can easily check the ambiguity: if time is equal or less 00:03:56
-- you can get the second time by adding 23:56:04
gstToUT :: JulianDate -> JulianDate
gstToUT jd =
  let (JD day, JD time) = splitToDayAndTime jd
      t = solarSiderealTimesDiff day
      time' = (reduceToZeroRange 24 (time*24-t)) * siderealDayLength
  in JD $ day + time'/24


-- | Convert Global Sidereal Time to Local Sidereal Time.
-- It takes GST and longitude in decimal degrees
gstToLST :: C.DecimalDegrees -> JulianDate -> JulianDate
gstToLST longitude jd =
  let (JD day, JD time) = splitToDayAndTime jd
      C.DH dhours = C.toDecimalHours longitude
      time' = time + dhours/24
  in JD $ day + time'


-- | Convert Local Sidereal Time to Global Sidereal Time.
-- It takes LST and longitude in decimal degrees
lstToGST :: C.DecimalDegrees -> JulianDate -> JulianDate
lstToGST longitude jd =
  let (JD day, JD time) = splitToDayAndTime jd
      C.DH dhours = C.toDecimalHours longitude
      time' = time - dhours/24
  in JD $ day + time'


-- Sidereal time internal functions

-- sidereal 24h correspond to 23:56:04 of solar time
siderealDayLength :: BaseType
siderealDayLength = toDecimalHours (TimeOfDay 23 56 04.0916)


solarSiderealTimesDiff :: BaseType -> BaseType
solarSiderealTimesDiff d =
  let t = (d - 2451545.0)/36525.0
  in 6.697374558 + 2400.051336*t + 0.000025862*t*t
