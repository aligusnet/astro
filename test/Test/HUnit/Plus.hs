module Test.HUnit.Plus
(
    assertMaybeDouble
  , assertOnFunction
)
where

import Test.HUnit
import Test.HUnit.Approx

assertMaybeDouble :: Maybe Double -> Maybe Double -> Double -> Assertion
assertMaybeDouble Nothing Nothing _ = assertString ""
assertMaybeDouble expected Nothing _ = assertString msg
  where msg = "expected: " ++ show expected ++ "\nbut got: Nothing"
assertMaybeDouble Nothing actual _ = assertString msg
  where msg = "expected: Nothing\nbit got: " ++ show actual
assertMaybeDouble (Just expected) (Just actual) eps = assertApproxEqual "Maybe Double" eps expected actual

assertOnFunction :: (Eq b, Show b) => (a -> b) -> a -> a -> Assertion
assertOnFunction func expected actual = func expected @=? func actual
