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
  , GeographicCoordinates(..)
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


instance Num DecimalDegrees where
  (+) (DD d1) (DD d2) = DD (d1+d2)
  (-) (DD d1) (DD d2) = DD (d1-d2)
  (*) (DD d1) (DD d2) = DD (d1*d2)
  negate (DD d) = DD (negate d)
  abs (DD d) = DD (abs d)
  signum (DD d) = DD (signum d)
  fromInteger int = DD (fromInteger int)


newtype DecimalHours = DH Double
                       deriving (Show, Eq, Ord)


instance Num DecimalHours where
  (+) (DH d1) (DH d2) = DH (d1+d2)
  (-) (DH d1) (DH d2) = DH (d1-d2)
  (*) (DH d1) (DH d2) = DH (d1*d2)
  negate (DH d) = DH (negate d)
  abs (DH d) = DH (abs d)
  signum (DH d) = DH (signum d)
  fromInteger int = DH (fromInteger int)

instance Real DecimalHours where
  toRational (DH d) = toRational d

instance Fractional DecimalHours where
  (/) (DH d1) (DH d2) = DH (d1/d2)
  recip (DH d) = DH (recip d)
  fromRational r = DH (fromRational r)

instance RealFrac DecimalHours where
  properFraction (DH d) =
    let (i, f) = properFraction d
    in (i, DH f)


-- | Convert decimal degrees to decimal hours
toDecimalHours :: DecimalDegrees -> DecimalHours
toDecimalHours (DD d) = DH $ d/15  -- 360 / 24 = 15

-- | Convert decimal hours to decimal degrees
fromDecimalHours :: DecimalHours -> DecimalDegrees
fromDecimalHours (DH h) = DD $ h*15


-- | Geographic Coordinates
data GeographicCoordinates = GeoC {
  geoLatitude :: DecimalDegrees
  , geoLongitude :: DecimalDegrees
  } deriving (Show, Eq)


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

