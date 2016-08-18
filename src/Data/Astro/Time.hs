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
import Data.Astro.Time.JulianDate (JulianDate, LocalCivilTime(..), LocalCivilDate(..))
import Data.Astro.Time.Sidereal (LocalSiderealTime, utToGST, gstToUT, gstToLST, lstToGST)


-- | Local Civil Time to Local Sidereal Time.
-- It takes longitude in decimal degrees and local civil time
lctToLST :: DecimalDegrees -> LocalCivilTime -> LocalSiderealTime
lctToLST longitude lct =
  let ut = lctUniversalTime lct
      gst = utToGST ut
      lst = gstToLST longitude gst
  in lst


-- | Local Sidereal Time to Local Civil Time.
-- It takes longitude in decimal degrees, local civil date and local sidereal time
lstToLCT :: DecimalDegrees -> LocalCivilDate -> LocalSiderealTime -> LocalCivilTime
lstToLCT longitude lcd lst =
  let gst = lstToGST longitude lst
      ut = gstToUT (lcdDate lcd) gst
      lct = LCT (lcdTimeZone lcd) ut
  in lct
