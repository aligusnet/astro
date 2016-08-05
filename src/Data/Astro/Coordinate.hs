module Data.Astro.Coordinate
(
  DecimalDegrees(..)
  , DecimalHours(..)
  , DegreeMS(..)
  , toDecimalDegrees
  , fromDecimalDegrees
  , toDecimalHours
)

where

import Data.Fixed (Pico)

import Data.Astro.Utils (fromFixed)

type DecimalDegrees = Double
type DecimalHours = Double


-- | Degrees, Minutes, Seconds
data DegreeMS = DegreeMS {
  dmsDegrees :: Int
  , dmsMinutes :: Int
  , dmsSeconds :: Pico
  } deriving (Show, Eq)


-- | Convert DegreeMS to DecimalDegree
toDecimalDegrees :: DegreeMS -> DecimalDegrees
toDecimalDegrees (DegreeMS d m s) =
  let d' = fromIntegral d
      m' = fromIntegral m
      s' = fromFixed s
  in d'+(m'+(s'/60))/60


-- | Convert from DecimalDegree to DegreeMS
fromDecimalDegrees :: DecimalDegrees -> DegreeMS
fromDecimalDegrees d =
  let (h, rm) = properFraction d
      (m, rs) = properFraction $ 60 * rm
      s = realToFrac $ 60 * rs
  in DegreeMS h m s


-- | Convert decimal degrees to decimal hours
toDecimalHours :: DecimalDegrees -> DecimalHours
toDecimalHours d = d/15  -- 360 / 24 = 15
