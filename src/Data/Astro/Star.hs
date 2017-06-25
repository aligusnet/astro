{-|
Module: Data.Astro.Star
Description: Stars
Copyright: Alexander Ignatyev, 2017

Stars.

= Examples

== /Location/

@
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
@

== /Rise and Set/

@
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
@
-}


module Data.Astro.Star
(
  Star(..)
  , starCoordinates
)

where

import Data.Astro.Coordinate (EquatorialCoordinates1(..))
import Data.Astro.Types (fromDMS, fromHMS)


-- | Some of the stars
data Star = Polaris
            | AlphaCrucis
            | Sirius
            | Betelgeuse
            | Rigel
            | Vega
            | Antares
            | Canopus
            | Pleiades
              deriving (Show, Eq)


-- | Returns Equatorial Coordinates for the given star
starCoordinates :: Star -> EquatorialCoordinates1
starCoordinates Polaris = EC1 (fromDMS 89 15 51) (fromHMS 2 31 48.7)
starCoordinates AlphaCrucis = EC1 (-(fromDMS 63 5 56.73)) (fromHMS 12 26 35.9)
starCoordinates Sirius = EC1 (-(fromDMS 16 42 58.02)) (fromHMS 6 45 8.92)
starCoordinates Betelgeuse = EC1 (fromDMS 07 24 25.4304) (fromHMS 5 55 10.30536)
starCoordinates Rigel = EC1 (-(fromDMS 8 12 05.8981)) (fromHMS 5 14 32.27210)
starCoordinates Vega = EC1 (fromDMS 38 47 01.2802) (fromHMS 18 36 56.33635)
starCoordinates Antares = EC1 (-(fromDMS 26 25 55.2094)) (fromHMS 16 29 24.45970)
starCoordinates Canopus = EC1 (-(fromDMS 52 41 44.3810)) (fromHMS 6 23 57.10988)
starCoordinates Pleiades = EC1 (fromDMS 24 7 00) (fromHMS 3 47 24)
