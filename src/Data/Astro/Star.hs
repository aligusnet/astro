{-|
Module: Data.Astro.Star
Description: Stars
Copyright: Alexander Ignatyev, 2017

Stars.
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

