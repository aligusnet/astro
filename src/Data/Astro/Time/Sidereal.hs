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
  GreenwichSiderealTime
  , LocalSiderealTime
  , dhToGST
  , dhToLST
  , gstToDH
  , lstToDH
  , hmsToGST
  , hmsToLST
  , utToGST
  , gstToUT
  , gstToLST
  , lstToGST
  , lstToGSTwDC
)
where

import Data.Astro.Types (DecimalHours(..), fromHMS)
import Data.Astro.Time.JulianDate (JulianDate(..), TimeBaseType, numberOfCenturies, splitToDayAndTime)
import Data.Astro.Time.Epoch (j2000)
import Data.Astro.Utils (reduceToZeroRange)
import qualified Data.Astro.Types as C


-- | Greenwich Sidereal Time
-- GST can be in range [-12h, 36h] carrying out a day correction
newtype GreenwichSiderealTime = GST TimeBaseType deriving (Show, Eq)


-- | Local Sidereal Time
newtype LocalSiderealTime = LST TimeBaseType deriving (Show, Eq)


-- | Convert Decimal Hours to Greenwich Sidereal Time
dhToGST :: DecimalHours -> GreenwichSiderealTime
dhToGST (DH t) = GST t


-- | Convert Decimal Hours to Local Sidereal Time
dhToLST :: DecimalHours -> LocalSiderealTime
dhToLST (DH t) = LST t


-- | Convert Greenwich Sidereal Time to Decimal Hours
gstToDH :: GreenwichSiderealTime -> DecimalHours
gstToDH (GST t) = DH t


-- | Convert Local Sidereal Time to Decimal Hours
lstToDH :: LocalSiderealTime -> DecimalHours
lstToDH (LST t) = DH t


-- | Comvert Hours, Minutes, Seconds to Greenwich Sidereal Time
hmsToGST :: Int -> Int -> TimeBaseType -> GreenwichSiderealTime
hmsToGST h m s = dhToGST $ fromHMS h m s


-- | Comvert Hours, Minutes, Seconds to Local Sidereal Time
hmsToLST :: Int -> Int -> TimeBaseType -> LocalSiderealTime
hmsToLST h m s = dhToLST $ fromHMS h m s


-- | Convert from Universal Time (UT) to Greenwich Sidereal Time (GST)
utToGST :: JulianDate -> GreenwichSiderealTime
utToGST jd =
  let (JD day, JD time) = splitToDayAndTime jd
      t = solarSiderealTimesDiff day
      time' = reduceToZeroRange 24 $ time*24/siderealDayLength + t
  in GST $ time'


-- | Convert from Greenwich Sidereal Time (GST) to Universal Time (UT).
-- It takes GST and Greenwich Date, returns JulianDate.
-- Because the sidereal day is shorter than the solar day (see comment to the module).
-- In case of such ambiguity the early time will be returned.
-- You can easily check the ambiguity: if time is equal or less 00:03:56
-- you can get the second time by adding 23:56:04
gstToUT :: JulianDate -> GreenwichSiderealTime -> JulianDate
gstToUT jd gst =
  let (day, time) = dayTime jd gst
      t = solarSiderealTimesDiff day
      time' = (reduceToZeroRange 24 (time-t)) * siderealDayLength
  in JD $ day + time'/24
  where dayTime jd (GST gst)
          | gst < 0   = (day-1, gst+24)
          | gst >= 24 = (day+1, gst-24)
          | otherwise = (day,   gst)
            where (JD day, _) = splitToDayAndTime jd


-- | Convert Greenwich Sidereal Time to Local Sidereal Time.
-- It takes GST and longitude in decimal degrees
gstToLST :: C.DecimalDegrees -> GreenwichSiderealTime -> LocalSiderealTime
gstToLST longitude (GST gst) =
  let C.DH dhours = C.toDecimalHours longitude
      lst = reduceToZeroRange 24 $ gst + dhours
  in LST lst


-- | Convert Local Sidereal Time to Greenwich Sidereal Time
-- It takes LST and longitude in decimal degrees
lstToGST :: C.DecimalDegrees -> LocalSiderealTime -> GreenwichSiderealTime
lstToGST longitude (LST lst) =
  let C.DH dhours = C.toDecimalHours longitude
      gst = reduceToZeroRange 24 $ lst - dhours
  in GST gst


-- | Convert Local Sidereal Time to Greenwich Sidereal Time with Day Correction.
-- It takes LST and longitude in decimal degrees
lstToGSTwDC :: C.DecimalDegrees -> LocalSiderealTime -> GreenwichSiderealTime
lstToGSTwDC longitude (LST lst) =
  let C.DH dhours = C.toDecimalHours longitude
      gst = lst - dhours
  in GST gst


-- Sidereal time internal functions

-- sidereal 24h correspond to 23:56:04 of solar time
siderealDayLength :: TimeBaseType
siderealDayLength = hours/24
  where C.DH hours = fromHMS 23 56 04.0916


solarSiderealTimesDiff :: TimeBaseType -> TimeBaseType
solarSiderealTimesDiff d =
  let t = numberOfCenturies j2000 (JD d)
  in reduceToZeroRange 24 $ 6.697374558 + 2400.051336*t + 0.000025862*t*t
