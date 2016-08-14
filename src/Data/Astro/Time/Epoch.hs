{-|
Module: Data.Astro.Time.Epoch
Description: Astronomical Epochs
Copyright: Alexander Ignatyev, 2016

Definitions of well-known astronomical epochs.
-}
module Data.Astro.Time.Epoch
(
    -- * Epochs
    -- ** Besselian Epochs
  b1900
  , b1950
    -- ** New Epochs
  , j1900
  , j2000
  , j2050
    -- ** Well-known epochs
  , j2010
)

where


import Data.Astro.Time.JulianDate (JulianDate(..))

-- | Epoch B1900.0, 1900 January 0.8135
b1900 :: JulianDate
b1900 = JD 2415020.3135

-- | Epoch B1950.0, January 0.9235
b1950 :: JulianDate
b1950 = JD 2433282.4235


-- | Epoch J1900.0 1900 January 0.5
j1900 :: JulianDate
j1900 = JD 2415020.0

-- | Epoch J2000.0, 12h on 1 January 2000
j2000 :: JulianDate
j2000 = JD 2451545.0

-- | Epoch J2050.0, 12h on 1 January 2000
j2050 :: JulianDate
j2050 = JD 2469807.50


-- | The Sun's and planets reference Epoch J2010.0 (2010 January 0.0)
j2010 :: JulianDate
j2010 = JD 2455196.5
