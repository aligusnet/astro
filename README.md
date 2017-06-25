# Amateur astronomical computations

[![Build Status](https://travis-ci.org/Alexander-Ignatyev/astro.svg?branch=master)](https://travis-ci.org/Alexander-Ignatyev/astro)
[![Coverage Status](https://coveralls.io/repos/github/Alexander-Ignatyev/astro/badge.svg)](https://coveralls.io/github/Alexander-Ignatyev/astro)
[![Documentation](https://img.shields.io/badge/mltool-documentation-blue.svg)](https://alexander-ignatyev.github.io/astro-docs/doc/index.html)


## Usage

### Build the project

    stack build

### Run unit tests

    stack tests

## Documentation

### Useful types

#### Decimal hours and Decimal degrees

Types to represent hours (used in celestial coordinate systems and as time zone) and degrees (used in coordinate systems).

```haskell
import Data.Astro.Types

-- 10h 15m 19.7s
dh :: DecimalHours
dh = fromHMS 10 15 19.7
-- DH 10.255472222222222

(h, m, s) = toHMS dh
-- (10,15,19.699999999999562)


-- 51°28′40″
dd :: DecimalDegrees
dd = fromDMS 51 28 40
-- DD 51.477777777777774

(d, m, s) = toDMS dd
-- (51,28,39.999999999987494)
```

#### Geographic Coordinates

```haskell
import Data.Astro.Types

-- the Royal Observatory, Greenwich
ro :: GeographicCoordinates
ro = GeoC (fromDMS 51 28 40) (-(fromDMS 0 0 5))
-- GeoC {geoLatitude = DD 51.4778, geoLongitude = DD (-0.0014)}
```

### Time

The main time datetime type used in the library is `JulianDate` defined in `Data.Astro.Time.JulianDate`. JulianDate is just a number of days since noon of 1 January 4713 BC:

```haskell
import Data.Astro.Time.JulianDate

-- 2017-06-25 9:29:00 (GMT)
jd :: JulianDate
jd = fromYMDHMS 2017 6 25 9 29 0
-- JD 2457929.895138889
```

`LocalCiviTime` and `LocalCivilDate` are Julian dates with time zones:

```haskell
import Data.Astro.Time.JulianDate
import Data.Astro.Types

-- 2017-06-25 10:29:00 +0100 (BST)
lct :: LocalCivilTime
lct = lctFromYMDHMS (DH 1) 2017 6 25 10 29 0
-- 2017-06-25 10:29:00.0000 +1.0

lctJD :: JulianDate
lctJD = lctUniversalTime lct
-- JD 2457929.895138889

lctTZ :: DecimalHours
lctTZ = lctTimeZone lct
-- DH 1.0

lcd :: LocalCivilDate
lcd = lcdFromYMD (DH 1) 2017 6 25

lcdJD :: JulianDate
lcdJD = lcdDate lcd
-- JD 2457929.5

lcdTZ :: DecimalHours
lcdTZ = lcdTimeZone lcd
-- DH 1.0
```

### Celestial coordinate systems

The celestical coordinate systems are defined in `Data.Astro.Coordinate`.

If you would like to locate Sirius in the sky you need to know the altitude or 'how far up' angle in the sky and azimuth - 'how far round' angle from the north direction to the east. this describes the __Horizontal coordinate system__:

![alt Horizontal coordinate system](hcs.svg "Horizontal coordinate system")


```haskell
import Data.Astro.Coordinate
import Data.Astro.Types

hc :: HorizonCoordinates
hc = HC (DD 30.5) (DD 180)
-- HC {hAltitude = DD 30.0, hAzimuth = DD 180.0}
```

Unfortunately the Horizontal coordinate system values depend on the position of the observer. And it's not handy when you need to share coordinates of some celestial object with your friend in Japan.

The second coordinate system is the __Equatorial coordinate system__. This coordinate system uses the location of the centre of the Earth as the zero point so it does not depend on the observer's location.

We have two flavours of equatorial coordinates:

* the first one uses the _vernal equinox_ as a starting direction for the 'how far round' coordinate (__right ascension, &#x3B1;__),

* the second one uses the _meridian_ instead of the vernal equinox (__hour angle__).

We can consider the second one as a transition coordinate system between the horizontal one and the 'true' equatorial one.


```haskell
import Data.Astro.Coordinate
import Data.Astro.Types

ec1 :: EquatorialCoordinates1
ec1 = EC1 (DD 71.7) (DH 8)
-- EC1 {e1Declination = DD 71.7, e1RightAscension = DH 8.0}

ec2 :: EquatorialCoordinates2
ec2 = EC1 (DD 77.7) (DH 11)
-- EC2 {e2Declination = DD 77.7, e2HoursAngle = DH 11.0}
```

#### Transformations

Say, now is 2017-06-25 10:29 BST and we are somewhere near the Royal Observatory, Greenwich.

Let convert the current location of the Sun in horizon coordinates (altitude: 49°18′21.77″, azimuth: 118°55′19.53″) to equatorial coordinates and back to horizon ones:

```haskell
import Data.Astro.Coordinate
import Data.Astro.Types

ro :: GeographicCoordinates
ro = GeoC (fromDMS 51 28 40) (-(fromDMS 0 0 5))

dt :: LocalCivilTime
dt = lctFromYMDHMS (DH 1) 2017 6 25 10 29 0

sunHC :: HorizonCoordinates
sunHC = HC (fromDMS 49 18 21.77) (fromDMS 118 55 19.53)
-- HC {hAltitude = DD 49.30604722222222, hAzimuth = DD 118.92209166666666}

sunEC2 :: EquatorialCoordinates2
sunEC2 = horizonToEquatorial (geoLatitude ro) sunHC
-- EC2 {e2Declination = DD 23.378295912623855, e2HoursAngle = DH 21.437117068873537}

sunEC1 :: EquatorialCoordinates1
sunEC1 = EC1 (e2Declination sunEC2) (haToRA (e2HoursAngle sunEC2) (geoLongitude ro) (lctUniversalTime dt))
-- EC1 {e1Declination = DD 23.378295912623855, e1RightAscension = DH 6.29383725890224}


sunEC2' :: EquatorialCoordinates2
sunEC2' = EC2 (e1Declination sunEC1) (raToHA (e1RightAscension sunEC1) (geoLongitude ro) (lctUniversalTime dt))
-- EC2 {e2Declination = DD 23.378295912623855, e2HoursAngle = DH 21.437117068873537}

sunHC' :: HorizonCoordinates
sunHC' = equatorialToHorizon (geoLatitude ro) sunEC2'
-- HC {hAltitude = DD 49.30604722222222, hAzimuth = DD 118.92209166666666}
```

### Stars

The ancient astronomers noted that there were 2 types of stars: some of them were fixed, travelling the same way across the sky every sidereal day and another were wanderers (planetai in ancient Greek).

Of course, stars are not fixed, they are travelling with high speeds but distances to them are so high that their movement is very difficult to note. So we can assume that they are fixed for our purposes.

Given the "fixed" equatorial coordinates of the star we only need to transform them to the horizon coordinates to find out where the star in the sky.

In the example below we will use `Data.Astro.Star` module which defines equatorial coordinates of some stars:

```haskell
import Data.Astro.Coordinate
import Data.Astro.Types
import Data.Astro.Star


ro :: GeographicCoordinates
ro = GeoC (fromDMS 51 28 40) (-(fromDMS 0 0 5))

dt :: LocalCivilTime
dt = lctFromYMDHMS (DH 1) 2017 6 25 10 29 0

-- Calculate location of Betelgeuse

betelgeuseEC1 :: EquatorialCoordinates1
betelgeuseEC1 = starCoordinates Betelgeuse
-- EC1 {e1Declination = DD 7.407064, e1RightAscension = DH 5.919529}

betelgeuseEC2 :: EquatorialCoordinates2
betelgeuseEC2 = EC2 (e1Declination betelgeuseEC1) (raToHA (e1RightAscension betelgeuseEC1) (geoLongitude ro) (lctUniversalTime dt))
-- EC2 {e2Declination = DD 7.407064, e2HoursAngle = DH 21.811425}

betelgeuseHC :: HorizonCoordinates
betelgeuseHC = equatorialToHorizon (geoLatitude ro) betelgeuseEC2
-- HC {hAltitude = DD 38.30483892505852, hAzimuth = DD 136.75755644642248}
```
