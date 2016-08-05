module Data.Astro.UtilsTest
(
  tests
)

where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.HUnit.Approx
import Test.QuickCheck

import Data.Fixed(Pico(..))

import Data.Astro.Utils

tests = [testGroup "fromFixed" [
            testProperty "property" prop_fromFixed
            , testCase "5.111123" $ assertApproxEqual "" 0.000000001 5.111123 $ fromFixed (toPico 5.111123)
            , testCase "-999.9999" $ assertApproxEqual "" 0.000000001 (-999.9999) $ fromFixed (toPico (-999.9999))
            ]
        , testGroup "fraction" [
            testProperty "property" prop_fraction
            , testCase "5.562" $ assertFraction 5.562
            , testCase "-7.93" $ assertFraction (-7.93)
            , testCase "-999.9999" $ assertFraction (-999.9999)
            ]
        , testGroup "trunc" [
            testProperty "property" prop_trunc
            , testCase "5.562" $ 5.0 @=? trunc 5.562
            , testCase "-7.93" $ (-7.0) @=? trunc (-7.93)
            ]
        , testGroup "reduceToZeroRange" [
            testCase "24 -465.986246" $ assertApproxEqual "" 0.000000001 14.013754 $ reduceToZeroRange 24 (-465.986246)
            , testProperty "property for r = 24" $ prop_reduceToZeroRange 24
            , testProperty "property for r = 1" $ prop_reduceToZeroRange 1
            ]
        ]

toPico :: Real a => a -> Pico
toPico = realToFrac

fromFraction :: Real a => (Int, a) -> a
fromFraction (i, f) = f + fromIntegral i

assertFraction d = assertApproxEqual "" 0.0000001 d $ fromFraction $ fraction d

prop_fromFixed d =
  abs((fromFixed ((realToFrac d)::Pico))-d) < 0.0000001
  where types = (d::Double)

prop_fraction d =
  let (i, f) = fraction d
      f' = d - fromIntegral i
  in i == truncate d && abs(f'-f) < 0.0000001
  where types = (d::Double)

prop_trunc d =
  let d' = fromIntegral $ truncate d
  in abs(d' - trunc d) < 0.0000001
  where types = (d::Double)

prop_reduceToZeroRange r n =
  let k = reduceToZeroRange r n
  in k < r && k >= 0
     where types = (n::Double)