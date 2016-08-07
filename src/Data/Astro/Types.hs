{-|
Module: Data.Astro.Types
Description: Common Types
Copyright: Alexander Ignatyev, 2016

Common Types are usfull across all subsystems like Time and Coordinate
-}

module Data.Astro.Types
(
  DecimalDegrees(..)
  , DecimalHours (..)
  , toDecimalHours
  , fromDecimalHours
  , toRadians
  , fromRadians
)

where

import qualified Data.Astro.Utils as U

newtype DecimalDegrees = DD Double
                         deriving (Show, Eq, Ord)

newtype DecimalHours = DH Double
                       deriving (Show, Eq, Ord)

     
-- | Convert decimal degrees to decimal hours
toDecimalHours :: DecimalDegrees -> DecimalHours
toDecimalHours (DD d) = DH $ d/15  -- 360 / 24 = 15

-- | Convert decimal hours to decimal degrees
fromDecimalHours :: DecimalHours -> DecimalDegrees
fromDecimalHours (DH h) = DD $ h*15


-- | Convert from DecimalDegrees to Radians
--toRadians :: Floating a => DecimalDegrees -> a
toRadians (DD deg) = U.toRadians deg


-- | Convert from Radians to DecimalDegrees
--fromRadians :: Floating a => a -> DecimalDegrees
fromRadians rad = DD $ U.fromRadians rad
