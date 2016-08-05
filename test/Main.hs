import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.Astro.TimeTest as Time
import qualified Data.Astro.Time.GregorianCalendarTest as Time.GregorianCalendar
import qualified Data.Astro.Time.JulianDateTest as Time.JulianDate
import qualified Data.Astro.Time.SiderealTest
import qualified Data.Astro.CoordinateTest
import qualified Data.Astro.UtilsTest

main = defaultMain tests

tests = [
  testGroup "Data.Astro.Time" Time.tests
  , testGroup "Data.Astro.Time.GregorianCalendar" Time.GregorianCalendar.tests
  , testGroup "Data.Astro.Time.JulianDate" Time.JulianDate.tests
  , testGroup "Data.Astro.Time.Sidereal" Data.Astro.Time.SiderealTest.tests
  , testGroup "Data.Astro.Coordinate" Data.Astro.CoordinateTest.tests
  , testGroup "Data.Astro.Utils" Data.Astro.UtilsTest.tests
  ]
