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

![alt Horizontal coordinate system](https://upload.wikimedia.org/wikipedia/commons/f/f7/Azimuth-Altitude_schematic.svg "Horizontal coordinate system")


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
import Data.Astro.Time.JulianDate
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

You can use function-shortcuts to simplify transformation EquatorialCoordinates1 <-> HorizonCoordinates: `ec1ToHC` and `hcToEC1`:

```haskell
import Data.Astro.Time.JulianDate
import Data.Astro.Coordinate
import Data.Astro.Types

ro :: GeographicCoordinates
ro = GeoC (fromDMS 51 28 40) (-(fromDMS 0 0 5))

dt :: LocalCivilTime
dt = lctFromYMDHMS (DH 1) 2017 6 25 10 29 0

sunHC :: HorizonCoordinates
sunHC = HC (fromDMS 49 18 21.77) (fromDMS 118 55 19.53)
-- HC {hAltitude = DD 49.30604722222222, hAzimuth = DD 118.92209166666666}

sunEC1 :: EquatorialCoordinates1
sunEC1 = hcToEC1 ro (lctUniversalTime dt) sunHC
-- EC1 {e1Declination = DD 23.378295912623855, e1RightAscension = DH 6.29383725890224}

sunHC' :: HorizonCoordinates
sunHC' = ec1ToHC ro (lctUniversalTime dt) sunEC1
-- HC {hAltitude = DD 49.30604722222222, hAzimuth = DD 118.92209166666666}
```

### Stars

The ancient astronomers noted that there were 2 types of stars: some of them were fixed, travelling the same way across the sky every sidereal day and another were wanderers (planetai in ancient Greek).

Of course, stars are not fixed, they are travelling with high speeds but distances to them are so high that their movement is very difficult to note. So we can assume that they are fixed for our purposes.

Given the "fixed" equatorial coordinates of the star we only need to transform them to the horizon coordinates to find out where the star in the sky.

In the example below we will use `Data.Astro.Star` module which defines equatorial coordinates of some stars:

```haskell
import Data.Astro.Time.JulianDate
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

betelgeuseHC :: HorizonCoordinates
betelgeuseHC = ec1ToHC ro (lctUniversalTime dt) betelgeuseEC1
-- HC {hAltitude = DD 38.30483892505852, hAzimuth = DD 136.75755644642248}
```

#### Rise and Set

`Data.Astro.CelestialObject.RiseSet` module defines `RiseSet` type to represent time and azimuth of rise and set.

Let calculate rise and set time of Rigel:

```haskell
import Data.Astro.Time.JulianDate
import Data.Astro.Coordinate
import Data.Astro.Types
import Data.Astro.Effects
import Data.Astro.CelestialObject.RiseSet
import Data.Astro.Star


ro :: GeographicCoordinates
ro = GeoC (fromDMS 51 28 40) (-(fromDMS 0 0 5))

today :: LocalCivilDate
today = lcdFromYMD (DH 1) 2017 6 25

-- Calculate location of Betelgeuse

rigelEC1 :: EquatorialCoordinates1
rigelEC1 = starCoordinates Rigel

verticalShift :: DecimalDegrees
verticalShift = refract (DD 0) 12 1012
-- DD 0.5660098245614035

rigelRiseSet :: RiseSetLCT
rigelRiseSet = riseAndSetLCT ro today verticalShift rigelEC1
-- RiseSet (2017-06-25 06:38:18.4713 +1.0,DD 102.51249855335433) (2017-06-25 17:20:33.4902 +1.0,DD 257.48750144664564)
```

As we can see Rigel rose today at 06:38:18 and will set at 17:20:33, azimuths of rise and set 102.51° and 257.49° correspondingly.

We used `refract` function of `Data.Astro.Effects` module with reasonable default parameters to calculate vertical shift.

### Planets

The planets is completely different story. We cannot assume that the planets have "fixed" location in equatorial coordinates like stars.

What we can do is to describe details of the planets' orbit and calculate their positions at any given moment.

Planets and planet details are defined in `Data.Astro.Planet` module. `j2010PlanetDetails` returns details for the given planet.
This module also defines `planetPosition`, `planetDistance1` and `planetAngularDiameter` to calculate position of the given planet, distance to the planet and angular size of the planet correspondingly.

`1` at the end of the `planetDistance1` means that this function uses not very precise method to do calculations. Sometimes there are `2`-methods available in the library, but not always.

Let us do some planets-related calculations.

Do some initialisation:

```haskell
import Data.Astro.Time.JulianDate
import Data.Astro.Coordinate
import Data.Astro.Types
import Data.Astro.Effects
import Data.Astro.CelestialObject.RiseSet
import Data.Astro.Planet

ro :: GeographicCoordinates
ro = GeoC (fromDMS 51 28 40) (-(fromDMS 0 0 5))

dt :: LocalCivilTime
dt = lctFromYMDHMS (DH 1) 2017 6 25 10 29 0

today :: LocalCivilDate
today = lcdFromYMD (DH 1) 2017 6 25

jupiterDetails :: PlanetDetails
jupiterDetails = j2010PlanetDetails Jupiter

earthDetails :: PlanetDetails
earthDetails = j2010PlanetDetails Earth

jupiterPosition :: JulianDate -> EquatorialCoordinates1
jupiterPosition = planetPosition planetTrueAnomaly1 jupiterDetails earthDetails

```

Calculate Jupiter's coordinates:

```haskell
jupiterEC1 :: EquatorialCoordinates1
jupiterEC1 = jupiterPosition (lctUniversalTime dt)
-- EC1 {e1Declination = DD (-4.104626810672402), e1RightAscension = DH 12.863365504382228}

jupiterHC :: HorizonCoordinates
jupiterHC = ec1ToHC ro (lctUniversalTime dt) jupiterEC1
-- HC {hAltitude = DD (-30.67914598469227), hAzimuth = DD 52.29376845044007}
```

As be can see Jupiter is below the horizon now (the altitude is negative), that's unfortunate.

Now let us calculate distance to Jupiter:

```haskell
jupiterDistance :: AstronomicalUnits
jupiterDistance = planetDistance1 jupiterDetails earthDetails (lctUniversalTime dt)
-- AU 5.193435872521039
```

1 Astronomical Unit is an average distance from the Earth to the Sun.

and calculate an angular size now:

```haskell
jupiterAngularSize :: DecimalDegrees
jupiterAngularSize = planetAngularDiameter jupiterDetails jupiterDistance
-- DD 1.052289877865987e-2

toDMS jupiterAngularSize
-- (0,0,37.88243560317554)
```

#### Rise and Set

Calculate rise and set times of planets are not easy task, because planets change their equatorial coordinates during the day.

`riseAndSet2` function of `Data.Astro.CelestialObject.RiseSet` module applies iterative approach: calculates rise and set date for midday coordinates and then recalculates rise time for rise coordinates and set for set coordinates obtained from the previous step:

```haskell
verticalShift :: DecimalDegrees
verticalShift = refract (DD 0) 12 1012
-- DD 0.5660098245614035

jupiterRiseSet :: RiseSetMB
jupiterRiseSet = riseAndSet2 0.000001 jupiterPosition ro verticalShift today
-- RiseSet
--    (Just (2017-06-25 13:53:27.3109 +1.0,DD 95.88943953535569))
--    (Just (2017-06-25 01:21:23.5835 +1.0,DD 264.1289033612776))
```

We can see now why at 10 am Jupiter is below horizon because it will rise only at 1:53 pm.


### Sun

Some examples of doing the Sun's related calculations:

```haskell
import Data.Astro.Time.JulianDate
import Data.Astro.Coordinate
import Data.Astro.Types
import Data.Astro.Sun

ro :: GeographicCoordinates
ro = GeoC (fromDMS 51 28 40) (-(fromDMS 0 0 5))

dt :: LocalCivilTime
dt = lctFromYMDHMS (DH 1) 2017 6 25 10 29 0

today :: LocalCivilDate
today = lcdFromYMD (DH 1) 2017 6 25

jd :: JulianDate
jd = lctUniversalTime dt

verticalShift :: DecimalDegrees
verticalShift = refract (DD 0) 12 1012

-- distance from the Earth to the Sun in kilometres
distance :: Double
distance = sunDistance jd
-- 1.5206375976421073e8

-- Angular Size
angularSize :: DecimalDegrees
angularSize = sunAngularSize jd
-- DD 0.5244849215333616

-- The Sun's coordinates
ec1 :: EquatorialCoordinates1
ec1 = sunPosition2 jd
-- EC1 {e1Declination = DD 23.37339098989099, e1RightAscension = DH 6.29262026252748}

hc :: HorizonCoordinates
hc = ec1ToHC ro jd ec1
-- HC {hAltitude = DD 49.312050979507404, hAzimuth = DD 118.94723825710143}


-- Rise and Set
riseSet :: RiseSetMB
riseSet = sunRiseAndSet ro 0.833333 today
-- RiseSet
--    (Just (2017-06-25 04:44:04.3304 +1.0,DD 49.043237261724215))
--    (Just (2017-06-25 21:21:14.4565 +1.0,DD 310.91655607595595))
```


### Moon

The Moon's related calculations. `Data.Astro.Moon` module defines 2 new types of functions we haven't seen before: `moonPhase` and `moonBrightLimbPositionAngle` which calculate the phase (the area of the visible segment expressed as a fraction of the whole disk) and the position-angle which is the angle of the midpoint of the illuminated limb measured eastwards from the north point of the disk.


```haskell
import Data.Astro.Time.JulianDate
import Data.Astro.Coordinate
import Data.Astro.Types
import Data.Astro.Effects
import Data.Astro.CelestialObject.RiseSet
import Data.Astro.Moon

ro :: GeographicCoordinates
ro = GeoC (fromDMS 51 28 40) (-(fromDMS 0 0 5))

dt :: LocalCivilTime
dt = lctFromYMDHMS (DH 1) 2017 6 25 10 29 0

today :: LocalCivilDate
today = lcdFromYMD (DH 1) 2017 6 25

jd :: JulianDate
jd = lctUniversalTime dt

-- distance from the Earth to the Moon in kilometres
mdu :: MoonDistanceUnits
mdu = moonDistance1 j2010MoonDetails jd
-- MDU 0.9550170577020396

distance :: Double
distance = mduToKm mdu
-- 367109.51199772174

-- Angular Size
angularSize :: DecimalDegrees
angularSize = moonAngularSize mdu
-- DD 0.5425033990980761

-- The Moon's coordinates
position :: JulianDate -> EquatorialCoordinates1
position = moonPosition1 j2010MoonDetails

ec1 :: EquatorialCoordinates1
ec1 = position jd
-- EC1 {e1Declination = DD 18.706180658927323, e1RightAscension = DH 7.56710547682055}

hc :: HorizonCoordinates
hc = ec1ToHC ro jd ec1
-- HC {hAltitude = DD 34.57694951316064, hAzimuth = DD 103.91119101451832}

-- Rise and Set
riseSet :: RiseSetMB
riseSet = riseAndSet2 0.000001 position ro verticalShift today
-- RiseSet
--    (Just (2017-06-25 06:22:51.4858 +1.0,DD 57.81458864497365))
--    (Just (2017-06-25 22:28:20.3023 +1.0,DD 300.4168238905249))

-- Phase
phase :: Double
phase = moonPhase j2010MoonDetails jd
-- 2.4716141948212922e-2


sunEC1 :: EquatorialCoordinates1
sunEC1 = sunPosition2 jd
-- EC1 {e1Declination = DD 23.37339098989099, e1RightAscension = DH 6.29262026252748}

limbAngle :: DecimalDegrees
limbAngle = moonBrightLimbPositionAngle ec1 sunEC1
-- DD 287.9869373767473
```
