{-|
Module: Data.Astro.Time
Description: Time
Copyright: Alexander Ignatyev, 2016

Root Time module
-}


module Data.Astro.Time
(
  lctToLST
  , lstToLCT
)

where

import Data.Astro.Types (DecimalDegrees)
import Data.Astro.Time.JulianDate (JulianDate, utToLCT, lctToUT)
import Data.Astro.Time.Sidereal (LocalSiderealTime, utToGST, gstToUT, gstToLST, lstToGST)


-- | Local Civil Time to Local Sidereal Time.
-- It takes longitude in decimal degrees, time zone and local civil time
lctToLST :: DecimalDegrees -> Double -> JulianDate -> LocalSiderealTime
lctToLST longitude timeZone lct =
  let ut = lctToUT timeZone lct
      gst = utToGST ut
      lst = gstToLST longitude gst
  in lst


-- | Local Sidereal Time to Local Civil Time.
-- It takes longitude in decimal degrees, time zone, greenwich date and local sidereal time
lstToLCT :: DecimalDegrees -> Double -> JulianDate -> LocalSiderealTime -> JulianDate
lstToLCT longitude timeZone jd lst =
  let gst = lstToGST longitude lst
      ut = gstToUT jd gst
      lct = utToLCT timeZone ut
  in lct

