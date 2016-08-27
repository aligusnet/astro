{-|
Module: Data.Astro.Moon.MoonDetails
Description: Planet Details
Copyright: Alexander Ignatyev, 2016

Moon Details.
-}

module Data.Astro.Moon.MoonDetails
(
  MoonDetails(..)
  , MoonDistanceUnits(..)
  , j2010MoonDetails
  , mduToKm
)

where

import Data.Astro.Types (DecimalDegrees)
import Data.Astro.Time.Epoch (j2010)
import Data.Astro.Time.JulianDate (JulianDate(..))


-- | Details of the Moon's orbit at the epoch
data MoonDetails = MoonDetails {
  mdEpoch :: JulianDate     -- ^ the epoch
  , mdL :: DecimalDegrees   -- ^ mean longitude at the epoch
  , mdP :: DecimalDegrees   -- ^ mean longitude of the perigee at the epoch
  , mdN :: DecimalDegrees   -- ^ mean longitude of the node at the epoch
  , mdI :: DecimalDegrees   -- ^ inclination of the orbit
  , mdE :: Double           -- ^ eccentricity of the orbit
  , mdA :: Double           -- ^ semi-major axis of the orbit
  , mdBigTheta :: DecimalDegrees  -- ^ angular diameter at the distance `mdA` from the Earth
  , mdPi :: DecimalDegrees        -- ^ parallax at distance `mdA` from the Earth
  } deriving (Show)


-- | Moon distance units, 1 MDU = semi-major axis of the Moon's orbit
newtype MoonDistanceUnits = MDU Double deriving (Show)


j2010MoonDetails = MoonDetails j2010 91.929336 130.143076 291.682547 5.145396 0.0549 384401 0.5181 0.9507


-- | Convert MoonDistanceUnits to km
mduToKm :: MoonDistanceUnits -> Double
mduToKm (MDU p) = p * (mdA j2010MoonDetails)
