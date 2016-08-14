{-|
Module: Data.Astro.Coordinate
Description: Celestial Coordinate Systems
Copyright: Alexander Ignatyev, 2016

See "Data.Astro.Types" module for Georgraphic Coordinates.

= Celestial Coordinate Systems

== /Horizon coordinates/

* __altitude, &#x3B1;__ - /'how far up'/ angle from the horizontal plane in degrees
* __azimuth,  &#x391;__ - /'how far round'/ agle from the north direction in degrees to the east


== /Equatorial coordinates/

Accoring to the equatorial coordinates system stars move westwards along the circles centered in the north selestial pole,
making the full cicrle in 24 hours of sidereal time (see "Data.Astro.Time.Sidereal").

* __declination, &#x3B4;__ - /'how far up'/ angle from the quatorial plane;
* __right ascension, &#x3B1;__  - /'how far round'/ angle from the /vernal equinox/ to the east; __/or/__
* __hour angle__ - /'how far round'/ angle from the meridian to the west


== /Ecliptic Coordinate/

Accoring to the ecliptic coordinates system the Sun moves eastwards along the trace of th ecliptic. The Sun's ecplitic latitude is always 0.

* __ecliptic latitude, &#x3B2;__ - /'how far up'/ angle from the ecliptic
* __ecliptic longitude, &#x3BB;__ - /'how far round'/ angle from the /vernal equinox/ to the east


== /Galactic Coordinates/

* __galactic latitute, b__ - /'how far up'/ angle from the plane of the Galaxy
* __galactiv longitude, l__ - - /'how far round'/ angle from the direction the Sun - the centre of the Galaxy


== /Terms/

* __ecliptic__ - the plane containing the Earth's orbit around the Sun
* __vernal equinox__, &#x2648; - fixed direction lies along the line of the intersection of the equatorial plane and the ecliptic
* __obliquity of the ecliptic, &#x3B2;__ - the angle between the plane of the Earth's equator and the ecliptic
* __north selestial pole, P__ - the point on the selestial sphere, right above the Earth's North Pole

-}

module Data.Astro.Coordinate
(
  DecimalDegrees(..)
  , DecimalHours(..)
  , HorizonCoordinates(..)
  , EquatorialCoordinates1(..)
  , EquatorialCoordinates2(..)
  , EclipticCoordinates(..)
  , GalacticCoordinates(..)
  , raToHA
  , haToRA
  , equatorialToHorizon
  , horizonToEquatorial
  , ecHCConv
  , obliquity
  , eclipticToEquatorial
  , equatorialToEcliptic
  , galacticToEquatorial
  , equatorialToGalactic
)

where

import Data.Astro.Time (lctToLST)
import Data.Astro.Time.JulianDate (JulianDate(..), numberOfCenturies, splitToDayAndTime)
import Data.Astro.Time.Epoch (j2000)
import Data.Astro.Time.Sidereal (LocalSiderealTime(..), lstToDH)
import Data.Astro.Types (DecimalDegrees(..), DecimalHours(..), fromDecimalHours, toDecimalHours, toRadians, fromRadians, fromDMS)
import Data.Astro.Utils (fromFixed)
import Data.Astro.Effects.Nutation (nutationObliquity)


-- | Horizon Coordinates, for details see the module's description
data HorizonCoordinates = HC {
  hAltitude :: DecimalDegrees   -- ^ alpha
  , hAzimuth :: DecimalDegrees  -- ^ big alpha
  } deriving (Show, Eq)


-- | Equatorial Coordinates, defines fixed position in the sky
data EquatorialCoordinates1 = EC1 {
  e1Declination :: DecimalDegrees     -- ^ delta
  , e1RightAscension :: DecimalHours  -- ^ alpha
  } deriving (Show, Eq)


-- | Equatorial Coordinates
data EquatorialCoordinates2 = EC2 {
  e2Declination :: DecimalDegrees    -- ^ delta
  , e2HoursAngle :: DecimalHours     -- ^ H
  } deriving (Show, Eq)


-- | Ecliptic Coordinates
data EclipticCoordinates = EcC {
  ecLatitude :: DecimalDegrees      -- ^ beta
  , ecLongitude :: DecimalDegrees   -- ^ lambda
  } deriving (Show, Eq)


-- | Galactic Coordinates
data GalacticCoordinates = GC {
  gLatitude :: DecimalDegrees       -- ^ b
  , gLongitude :: DecimalDegrees    -- ^ l
  } deriving (Show, Eq)


-- | Convert Right Ascension to Hour Angle for specified longitude, time zone and Julian Date
raToHA :: DecimalHours -> DecimalDegrees -> Double -> JulianDate -> DecimalHours
raToHA = haRAConv


-- | Convert Hour Angle to Right Ascension for specified longitude, time zone and Julian Date
haToRA :: DecimalHours -> DecimalDegrees -> Double -> JulianDate -> DecimalHours
haToRA = haRAConv


-- | HA <-> RA Conversions
haRAConv :: DecimalHours -> DecimalDegrees -> Double -> JulianDate -> DecimalHours
haRAConv dh longitude tz jd =
  let lst = lctToLST longitude tz jd  -- Local Sidereal Time
      DH hourAngle = (lstToDH lst) - dh
  in if hourAngle < 0 then (DH $ hourAngle+24) else (DH hourAngle)


-- | Convert Equatorial Coordinates to Horizon Coordinates.
-- It takes a latitude of the observer and 'EquatorialCoordinates2'. If you need to convert 'EquatorialCoordinates1'
-- you should use 'raToHa' function to obtain 'EquatorialCoordinates2'.
-- The functions returns 'HorizonCoordinates'.
equatorialToHorizon :: DecimalDegrees -> EquatorialCoordinates2 -> HorizonCoordinates
equatorialToHorizon latitude (EC2 dec hourAngle) =
  let hourAngle' = fromDecimalHours hourAngle
      (altitude, azimuth) = ecHCConv latitude (dec, hourAngle')
  in HC altitude azimuth


-- | Convert Horizon Coordinates to Equatorial Coordinates.
-- It takes a latitude of the observer and 'HorizonCoordinates'.
-- The functions returns 'EquatorialCoordinates2'.
-- If you need to obtain 'EquatorialCoordinates1' you should use 'haToRa' function.
horizonToEquatorial :: DecimalDegrees -> HorizonCoordinates -> EquatorialCoordinates2
horizonToEquatorial latitude (HC altitude azimuth) =
  let (dec, hourAngle) = ecHCConv latitude (altitude, azimuth)
  in EC2 dec $ toDecimalHours hourAngle


-- | Function converts Equatorial Coordinates To Horizon Coordinates and vice versa
-- It takes a latitide of the observer as a first parameter and a pair of 'how far up' and 'how far round' coordinates
-- as a second parameter. It returns a pair of 'how far up' and 'how far round' coordinates.
ecHCConv :: DecimalDegrees -> (DecimalDegrees, DecimalDegrees) -> (DecimalDegrees, DecimalDegrees)
ecHCConv latitude (up, round) =
  let latitude' = toRadians latitude
      up' = toRadians up
      round' = toRadians round
      sinUpResult = (sin up')*(sin latitude') + (cos up')*(cos latitude')*(cos round')
      upResult = asin sinUpResult
      roundResult = acos $ ((sin up') - (sin latitude')*sinUpResult) / ((cos latitude') * (cos upResult))
      roundResult' = if (sin round') < 0 then roundResult else (2*pi - roundResult)
  in ((fromRadians upResult), (fromRadians roundResult'))


-- | Calculate the obliquity of the ecpliptic on JulianDate
obliquity :: JulianDate -> DecimalDegrees
obliquity jd =
  let DD baseObliquity = fromDMS 23 26 21.45
      t = numberOfCenturies j2000 jd
      de = (46.815*t + 0.0006*t*t - 0.00181*t*t*t) / 3600  -- 3600 number of seconds in 1 degree
  in (DD $ baseObliquity - de) + (nutationObliquity jd)


-- | Converts Ecliptic Coordinates on specified Julian Date to Equatorial Coordinates
eclipticToEquatorial :: EclipticCoordinates -> JulianDate -> EquatorialCoordinates1
eclipticToEquatorial (EcC beta gamma) jd =
  let epsilon' = toRadians $ obliquity jd
      beta' = toRadians beta
      gamma' = toRadians gamma
      delta = asin $ (sin beta')*(cos epsilon') + (cos beta')*(sin epsilon')*(sin gamma')
      y = (sin gamma')*(cos epsilon') - (tan beta')*(sin epsilon')
      x = cos gamma'
      alpha = reduceToZero2PI $ atan2 y x
  in EC1 (fromRadians delta) (toDecimalHours $ fromRadians alpha)


-- | Converts Equatorial Coordinates to Ecliptic Coordinates on specified Julian Date 
equatorialToEcliptic :: EquatorialCoordinates1 -> JulianDate -> EclipticCoordinates
equatorialToEcliptic (EC1 delta alpha) jd =
  let epsilon' = toRadians $ obliquity jd
      delta' = toRadians delta
      alpha' = toRadians $ fromDecimalHours alpha
      beta = asin $ (sin delta')*(cos epsilon') - (cos delta')*(sin epsilon')*(sin alpha')
      y = (sin alpha')*(cos epsilon') + (tan delta')*(sin epsilon')
      x = cos alpha'
      gamma = reduceToZero2PI $ atan2 y x
  in EcC (fromRadians beta) (fromRadians gamma)


-- | Galactic Pole Coordinates
galacticPole :: EquatorialCoordinates1
galacticPole = EC1 (DD 27.4) (toDecimalHours $ DD 192.25)

galacticPoleInRadians = (delta, alpha)
  where delta = toRadians $ e1Declination galacticPole
        alpha = toRadians $ fromDecimalHours $ e1RightAscension galacticPole


-- | Ascending node of the galactic place on equator
ascendingNode :: DecimalDegrees
ascendingNode = DD 33


-- | Convert Galactic Coordinates Equatorial Coordinates
galacticToEquatorial :: GalacticCoordinates -> EquatorialCoordinates1
galacticToEquatorial (GC b l) =
  let b' = toRadians b
      l' = toRadians l
      (poleDelta, poleAlpha) = galacticPoleInRadians
      an = toRadians ascendingNode
      delta = asin $ (cos b')*(cos poleDelta)*(sin (l'-an)) + (sin b')*(sin poleDelta)
      y = (cos b')*(cos (l'-an))
      x = (sin b')*(cos poleDelta) - (cos b')*(sin poleDelta)*(sin (l'-an))
      alpha = reduceToZero2PI $ (atan2 y x) + poleAlpha
  in EC1 (fromRadians delta) (toDecimalHours $ fromRadians alpha)


-- | Convert Equatorial Coordinates to Galactic Coordinates
equatorialToGalactic :: EquatorialCoordinates1 -> GalacticCoordinates
equatorialToGalactic (EC1 delta alpha) =
  let delta' = toRadians delta
      alpha' = toRadians $ fromDecimalHours alpha
      (poleDelta, poleAlpha) = galacticPoleInRadians
      sinb = (cos delta')*(cos poleDelta)*(cos (alpha'-poleAlpha)) + (sin delta') * (sin poleDelta)
      y = (sin delta') - sinb*(sin poleDelta)
      x = (cos delta')*(sin (alpha'-poleAlpha))*(cos poleDelta)
      b = asin sinb
      l = reduceToZero2PI $ (atan2 y x) + (toRadians ascendingNode)
  in GC (fromRadians b) (fromRadians l)


-- | Reduce angle from [-pi, pi] to [0, 2*pi]
-- Usefull to correct results of atan2 for 'how far round' coordinates
reduceToZero2PI :: (Floating a, Ord a) => a -> a
reduceToZero2PI rad = if rad < 0 then rad + 2*pi else rad
