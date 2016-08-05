import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.Astro.TimeTest
import qualified Data.Astro.CoordinateTest
import qualified Data.Astro.UtilsTest

main = defaultMain tests

tests = [
  testGroup "Data.Astro.Time" Data.Astro.TimeTest.tests
  , testGroup "Data.Astro.Coordinate" Data.Astro.CoordinateTest.tests
  , testGroup "Data.Astro.Utils" Data.Astro.UtilsTest.tests
  ]
