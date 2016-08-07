{-|
Module: Data.Astro.Time.Types
Description: Common Date and Time Types
Copyright: Alexander Ignatyev, 2016

Coomon Date and Time Types
-}


module Data.Astro.Time.Types
(
  Day(..)
  , TimeOfDay(..)
  , LocalTime(..)
  , fromGregorian
  , toGregorian
)
where

import Data.Time.Calendar (Day(..), fromGregorian, toGregorian)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
