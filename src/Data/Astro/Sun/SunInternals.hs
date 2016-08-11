{-|
Module: Data.Astro.Sun.SunInternals
Description: Internal functions of Sun module.
Copyright: Alexander Ignatyev, 2016

Internal functions of Sun module. Exposed only for Unit Tests
-}

module Data.Astro.Sun.SunInternals
(
  solveKeplerEquation
)

where


-- | Solve Kepler's Equation: E - e * (sin E) = M
-- It takes eccentricity,
-- mean anomaly in radians equals epsilon - omega (see 'SunDetails').
-- It returns E in radians.
solveKeplerEquation :: Double -> Double -> Double -> Double
solveKeplerEquation e m eps = iter m
  where iter x =
          let delta = x - e*(sin x) - m
              dx = delta / (1 - e*(cos x))
          in if abs delta < eps
             then x
             else iter (x-dx)
