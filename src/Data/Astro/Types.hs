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
)

where

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
