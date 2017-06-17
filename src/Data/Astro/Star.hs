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
              deriving (Show, Eq)


-- | Returns Equatorial Coordinates for the given star 
starCoordinates :: Star -> EquatorialCoordinates1
starCoordinates Polaris = EC1 (fromDMS 89 15 51) (fromHMS 2 31 48.7)
starCoordinates AlphaCrucis = EC1 (-(fromDMS 63 5 56.73)) (fromHMS 12 26 35.9)
starCoordinates Sirius = EC1 (-(fromDMS 16 42 58.02)) (fromHMS 6 45 8.92)

