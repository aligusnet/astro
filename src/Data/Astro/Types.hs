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
  , fromDMS
  , toDMS
  , fromHMS
  , toHMS
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
toRadians (DD deg) = U.toRadians deg


-- | Convert from Radians to DecimalDegrees
fromRadians rad = DD $ U.fromRadians rad


-- | Convert Degrees, Minutes, Seconds to DecimalDegrees
fromDMS :: RealFrac a => Int -> Int -> a -> DecimalDegrees
fromDMS d m s =
  let d' = fromIntegral d
      m' = fromIntegral m
      s' = realToFrac s
  in DD $ d'+(m'+(s'/60))/60


-- | Convert DecimalDegrees to Degrees, Minutes, Seconds
toDMS (DD dd) =
  let (d, rm) = properFraction dd
      (m, rs) = properFraction $ 60 * rm
      s = 60 * rs
  in (d, m, s)


-- | Comvert Hours, Minutes, Seconds to DecimalHours
fromHMS :: RealFrac a => Int -> Int -> a -> DecimalHours
fromHMS h m s =
  let h' = fromIntegral h
      m' = fromIntegral m
      s' = realToFrac s
  in DH $ h'+(m'+(s'/60))/60


-- | Convert DecimalDegrees to Degrees, Minutes, Seconds
toHMS (DH dh) =
  let (h, rm) = properFraction dh
      (m, rs) = properFraction $ 60 * rm
      s = 60 * rs
  in (h, m, s)

