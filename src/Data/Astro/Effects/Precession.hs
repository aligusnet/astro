{-|
Module: Data.Astro.Effects.Precession
Description: Luni-solar precession
Copyright: Alexander Ignatyev, 2016

Luni-solar precession.
-}

module Data.Astro.Effects.Precession
(
  AstronomyEpoch(..)
  , precession1
  , precession2
)

where

import Data.Matrix

import qualified Data.Astro.Utils as U
import Data.Astro.Types (DecimalDegrees(..), DecimalHours(..), toDecimalHours, fromDecimalHours, toRadians, fromRadians)
import Data.Astro.Time.JulianDate (JulianDate(..), numberOfYears, numberOfCenturies)
import Data.Astro.Time.Epoch (b1900, b1950, j2000, j2050)
import Data.Astro.Coordinate (EquatorialCoordinates1(..))


-------------------------------------------------------------------------------
-- Low-precision Precession

-- | Epoch Enumeration. See also "Data.Astro.Time.JulianDate" module.
data AstronomyEpoch = B1900  -- ^ Epoch B1900.0
                    | B1950  -- ^ Epoch B1950.0
                    | J2000  -- ^ Epoch J2000.0
                    | J2050  -- ^ Epoch J2050.0
                    deriving (Show, Eq)


-- | Get the start date of the specified Epoch.
epochToJD :: AstronomyEpoch -> JulianDate
epochToJD B1900 = b1900
epochToJD B1950 = b1950
epochToJD J2000 = j2000
epochToJD J2050 = j2050


-- | Precisional Constants
data PrecessionalConstants = PrecessionalConstants {
  pcM :: Double     -- ^ seconds
  , pcN :: Double   -- ^ seconds
  , pcN' :: Double  -- ^ arcsec
  }


-- | Get Precision Constants of the Epoch
precessionalConstants :: AstronomyEpoch -> PrecessionalConstants
precessionalConstants B1900 = PrecessionalConstants 3.07234 1.33645 20.0468
precessionalConstants B1950 = PrecessionalConstants 3.07327 1.33617 20.0426
precessionalConstants J2000 = PrecessionalConstants 3.07420 1.33589 20.0383
precessionalConstants J2050 = PrecessionalConstants 3.07513 1.33560 20.0340


-- | Low-precision method to calculate luni-solar precession.
-- It takes Epoch, Equatorial Coordinates those correct at the given epoch, Julian Date of the observation.
-- It returns corrected Equatorial Coordinates.
precession1 :: AstronomyEpoch -> EquatorialCoordinates1 -> JulianDate -> EquatorialCoordinates1
precession1 epoch (EC1 delta alpha) jd =
  let delta' = toRadians delta
      alpha' = toRadians $ fromDecimalHours alpha
      years = numberOfYears (epochToJD epoch) jd
      PrecessionalConstants m n n' = precessionalConstants epoch
      s1 = DH $ (m + n*(sin alpha')*(tan delta'))*years / 3600
      s2 = DD $ (n'*(cos alpha')) * years / 3600
  in (EC1 (delta + s2) (alpha + s1))


-------------------------------------------------------------------------------
-- Rigorous Method


-- | Rigorous method to calculate luni-solar precession.
-- It takes julian date at whose the coordinates are correct, Equatorial Coordinates, Julian Date of the observation.
-- It returns corrected Equatorial Coordinates.
precession2 :: JulianDate -> EquatorialCoordinates1 -> JulianDate -> EquatorialCoordinates1
precession2 epoch ec jd =
  let p' = prepareMatrixP' $ numberOfCenturies j2000 epoch
      v = prepareColumnVectorV ec
      p = transpose $ prepareMatrixP' $ numberOfCenturies j2000 jd
      [m, n, k] = toList $ p*(p'*v)
      alpha = atan2 n m
      delta = asin k
  in EC1 (fromRadians delta) (toDecimalHours $ fromRadians alpha)


prepareMatrixP' n =
  let x = U.toRadians $ 0.6406161*n + 0.0000839*n*n + 0.0000050*n*n*n
      z = U.toRadians $ 0.6406161*n + 0.0003041*n*n + 0.0000051*n*n*n
      t = U.toRadians $ 0.5567530*n - 0.0001185*n*n - 0.0000116*n*n*n
      cx = cos x
      sx = sin x
      cz = cos z
      sz = sin z
      ct = cos t
      st = sin t
      matrix = [ [cx*ct*cz-sx*sz,    cx*ct*sz+sx*cz,    cx*st]
               , [(-sx)*ct*cz-cx*sz, (-sx)*ct*sz+cx*cz, (-sx)*st]
               , [(-st)*cz,          (-st)*sz,          ct] ]
  in fromLists matrix

prepareColumnVectorV (EC1 delta alpha) =
  let d = toRadians delta
      a = toRadians $ fromDecimalHours alpha
      cd = cos d
      sd = sin d
      ca = cos a
      sa = sin a
      v = [ca*cd, sa*cd, sd]
  in fromList 3 1 v
