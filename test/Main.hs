import Test.Framework (defaultMain, testGroup)

import qualified Data.Astro.Time.GregorianCalendarTest as Time.GregorianCalendar
import qualified Data.Astro.Time.JulianDateTest as Time.JulianDate
import qualified Data.Astro.Time.SiderealTest as Time.Sidereal
import qualified Data.Astro.Time.TimeTest as Time.Time
import qualified Data.Astro.CoordinateTest as Coordinate
import qualified Data.Astro.UtilsTest as Utils

main = defaultMain tests

tests = [
    testGroup "Data.Astro.Time.GregorianCalendar" Time.GregorianCalendar.tests
  , testGroup "Data.Astro.Time.JulianDate" Time.JulianDate.tests
  , testGroup "Data.Astro.Time.Sidereal" Time.Sidereal.tests
  , testGroup "Data.Astro.Time.Time" Time.Time.tests
  , testGroup "Data.Astro.Coordinate" Coordinate.tests
  , testGroup "Data.Astro.Utils" Utils.tests
  ]
