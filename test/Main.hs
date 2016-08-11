import Test.Framework (defaultMain, testGroup)


import qualified Data.Astro.TimeTest as Time
import qualified Data.Astro.Time.GregorianCalendarTest as Time.GregorianCalendar
import qualified Data.Astro.Time.JulianDateTest as Time.JulianDate
import qualified Data.Astro.Time.SiderealTest as Time.Sidereal
import qualified Data.Astro.CoordinateTest as Coordinate
import qualified Data.Astro.TypesTest as Types
import qualified Data.Astro.UtilsTest as Utils
import qualified Data.Astro.CelestialObjectTest as CelestialObject
import qualified Data.Astro.EffectsTest as Effects
import qualified Data.Astro.SunTest as Sun
import qualified Data.Astro.Sun.SunInternalsTest as SunInternals

main = defaultMain tests

tests = [
  testGroup "Data.Astro.Time" Time.tests
  , testGroup "Data.Astro.Time.GregorianCalendar" Time.GregorianCalendar.tests
  , testGroup "Data.Astro.Time.JulianDate" Time.JulianDate.tests
  , testGroup "Data.Astro.Time.Sidereal" Time.Sidereal.tests
  , testGroup "Data.Astro.Coordinate" Coordinate.tests
  , testGroup "Data.Astro.Types" Types.tests
  , testGroup "Data.Astro.Utils" Utils.tests
  , testGroup "Data.Astro.CelestialObject" CelestialObject.tests
  , testGroup "Data.Astro.Effects" Effects.tests
  , testGroup "Data.Astro.Sun" Sun.tests
  , testGroup "Data.Astro.Sun.SunInternals" SunInternals.tests
  ]
