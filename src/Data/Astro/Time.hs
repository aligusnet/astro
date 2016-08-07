{-|
Module: Data.Astro.Time
Description: Time
Copyright: Alexander Ignatyev, 2016

Root Time module
-}


module Data.Astro.Time
(
  lctToLST
)

where

import Data.Astro.Types (DecimalDegrees)
import Data.Astro.Time.JulianDate (JulianDate, lctToUT)
import Data.Astro.Time.Sidereal (utToGST, gstToLST)


-- | Local Civil Time to Local Sidereal Time.Ty
-- It takes longitude in decimal degrees, time zone and local civil time
lctToLST :: DecimalDegrees -> Double -> JulianDate -> JulianDate
lctToLST longitude timeZone lct =
  let ut = lctToUT timeZone lct
      gst = utToGST ut
      lst = gstToLST longitude gst
  in lst
