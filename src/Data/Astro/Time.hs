{-|
Module: Data.Astro.Time
Description: Time
Copyright: Alexander Ignatyev, 2016

Root Time module
-}


module Data.Astro.Time
(
  utToLST
  , lctToLST
  , lstToLCT
)

where

import Data.Astro.Types (DecimalDegrees)
import Data.Astro.Time.JulianDate (JulianDate(..), LocalCivilTime(..), LocalCivilDate(..), splitToDayAndTime, addHours)
import Data.Astro.Time.Sidereal (LocalSiderealTime, utToGST, gstToUT, gstToLST, lstToGST, lstToGSTwDC)


-- | Universal Time to Local Sidereal Time.
-- It takes longitude in decimal degrees and local civil time
utToLST :: DecimalDegrees -> JulianDate -> LocalSiderealTime
utToLST longitude ut = gstToLST longitude $ utToGST ut


-- | Local Civil Time to Local Sidereal Time.
-- It takes longitude in decimal degrees and local civil time
lctToLST :: DecimalDegrees -> LocalCivilTime -> LocalSiderealTime
lctToLST longitude lct = utToLST longitude $ lctUniversalTime lct


-- | Local Sidereal Time to Local Civil Time.
-- It takes longitude in decimal degrees, local civil date and local sidereal time
lstToLCT :: DecimalDegrees -> LocalCivilDate -> LocalSiderealTime -> LocalCivilTime
lstToLCT longitude lcd lst =
  let gst = lstToGST longitude lst
      ut = gstToUT (lcdDate lcd) gst
      lct = LCT (lcdTimeZone lcd) ut
  in if sameDay lcd lct
     then lct -- lstToLCTwDC longitude timeZone jd lst
     else lstToLCTwDC longitude lcd lst


lstToLCTwDC :: DecimalDegrees -> LocalCivilDate -> LocalSiderealTime -> LocalCivilTime
lstToLCTwDC longitude lcd lst =
  let gst = lstToGSTwDC longitude lst
      ut = gstToUT (lcdDate lcd) gst
      lct = LCT (lcdTimeZone lcd) ut
  in lct


-- | Returns True if both JulianDates hve the same day
sameDay :: LocalCivilDate -> LocalCivilTime -> Bool
sameDay (LCD _ (JD d1)) (LCT tz jd2) =
  let (JD d2, _) = splitToDayAndTime $ addHours tz jd2
  in abs (d1 - d2) < 0.000001
