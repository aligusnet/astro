{-|
Module: Data.Astro.Coordinate
Description: Coordinate Systems
Copyright: Alexander Ignatyev, 2016

= Coordinate Systems

== /Horizon coordinates/

* __altitude, &#x3B1;__ - /'how far up'/ angle from the horizontal plane in degrees
* __azimuth,  &#x391;__ - /'how far round'/ agle from the north direction in degrees to the east

== /Equatorial coordinates/


Accoring to the equatorial coordinates system stars move westwards along the circles centered in the north selestial pole,
making the full cicrle in 24 hours of sidereal time (see "Data.Astro.Time.Sidereal").

* __declination, &#x3B4;__ - /'how far up'/ angle from the quatorial plane;
* __right ascension, &#x3B1;__  - /'how far round'/ angle from the /vernal equinox/ to the east; __/or/__
* __hour angle__ - /'how far round'/ angle from the meridian to the west

=== Other terms


* __vernal equinox__, &#x2648; - fixed direction lies along the line of the intersection of the equatorial plane and the ecliptic plane
* __north selestial pole, P__ - is a point on the selestial sphere, right above the Earth's North Pole

-}

module Data.Astro.Coordinate
(
  DecimalDegrees(..)
  , DecimalHours(..)
  , DegreeMS(..)
  , fromDegreeMS
  , toDegreeMS
  , toDecimalHours
  , fromDecimalHours
)

where

import Data.Fixed (Pico)

import Data.Astro.Utils (fromFixed)

newtype DecimalDegrees = DD Double
                         deriving (Show, Eq, Ord)

newtype DecimalHours = DH Double
                       deriving (Show, Eq, Ord)


-- | Degrees, Minutes, Seconds
data DegreeMS = DegreeMS {
  dmsDegrees :: Int
  , dmsMinutes :: Int
  , dmsSeconds :: Pico
  } deriving (Show, Eq)


-- | Convert DegreeMS to DecimalDegree
fromDegreeMS :: DegreeMS -> DecimalDegrees
fromDegreeMS (DegreeMS d m s) =
  let d' = fromIntegral d
      m' = fromIntegral m
      s' = fromFixed s
  in DD $ d'+(m'+(s'/60))/60


-- | Convert from DecimalDegree to DegreeMS
toDegreeMS :: DecimalDegrees -> DegreeMS
toDegreeMS (DD d) =
  let (h, rm) = properFraction d
      (m, rs) = properFraction $ 60 * rm
      s = realToFrac $ 60 * rs
  in DegreeMS h m s


-- | Convert decimal degrees to decimal hours
toDecimalHours :: DecimalDegrees -> DecimalHours
toDecimalHours (DD d) = DH $ d/15  -- 360 / 24 = 15

-- | Convert decimal hours to decimal degrees
fromDecimalHours :: DecimalHours -> DecimalDegrees
fromDecimalHours (DH h) = DD $ h*15
