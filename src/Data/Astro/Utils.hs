module Data.Astro.Utils
(
  fromFixed
  , trunc
  , fraction
  , reduceToZeroRange
)

where

import Data.Fixed(Fixed(MkFixed), HasResolution(resolution))

-- | Convert From Fixed to Fractional
fromFixed :: (Fractional a, HasResolution b) => Fixed b -> a
fromFixed fv@(MkFixed v) = (fromIntegral v) / (fromIntegral $ resolution fv)


-- | return the integral part of a number
-- almost the same as truncate but result type is Real
trunc :: RealFrac a => a -> a
trunc = fromIntegral . truncate


-- | Almost the same the properFraction function but result type
fraction :: (RealFrac a, Num b) => a -> (b, a)
fraction v = let (i, f) = (properFraction v)
             in (fromIntegral i, f)


-- | Reduce to range from 0 to n
reduceToZeroRange :: RealFrac a => a -> a -> a
reduceToZeroRange r n =
  let b = n - (trunc (n / r)) * r
  in if b < 0 then b + r else b
