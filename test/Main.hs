import Test.Framework (defaultMain, testGroup)


import qualified Data.Astro.TimeTest as Time
import qualified Data.Astro.Time.GregorianCalendarTest as Time.GregorianCalendar
import qualified Data.Astro.Time.JulianDateTest as Time.JulianDate
import qualified Data.Astro.Time.SiderealTest as Time.Sidereal
import qualified Data.Astro.Time.ConvTest as Time.Conv
import qualified Data.Astro.Time.EpochTest as Time.Epoch
import qualified Data.Astro.CoordinateTest as Coordinate
import qualified Data.Astro.TypesTest as Types
import qualified Data.Astro.UtilsTest as Utils
import qualified Data.Astro.CelestialObjectTest as CelestialObject
import qualified Data.Astro.CelestialObject.RiseSetTest as CelestialObject.RiseSet
import qualified Data.Astro.EffectsTest as Effects
import qualified Data.Astro.Effects.ParallaxTest as Effects.Parallax
import qualified Data.Astro.SunTest as Sun
import qualified Data.Astro.Sun.SunInternalsTest as SunInternals
import qualified Data.Astro.Planet.PlanetDetailsTest as PlanetDetails
import qualified Data.Astro.Planet.PlanetMechanicsTest as PlanetMechanics
import qualified Data.Astro.MoonTest as Moon


main = defaultMain tests

tests = [
  testGroup "Data.Astro.Time" Time.tests
  , testGroup "Data.Astro.Time.GregorianCalendar" Time.GregorianCalendar.tests
  , testGroup "Data.Astro.Time.JulianDate" Time.JulianDate.tests
  , testGroup "Data.Astro.Time.Sidereal" Time.Sidereal.tests
  , testGroup "Data.Astro.Time.Conv" Time.Conv.tests
  , testGroup "Data.Astro.Time.Epoch" Time.Epoch.tests
  , testGroup "Data.Astro.Coordinate" Coordinate.tests
  , testGroup "Data.Astro.Types" Types.tests
  , testGroup "Data.Astro.Utils" Utils.tests
  , testGroup "Data.Astro.CelestialObject" CelestialObject.tests
  , testGroup "Data.Astro.CelestialObject.RiseSet" CelestialObject.RiseSet.tests
  , testGroup "Data.Astro.Effects" Effects.tests
  , testGroup "Data.Astro.Effects.Parallax" Effects.Parallax.tests
  , testGroup "Data.Astro.Sun" Sun.tests
  , testGroup "Data.Astro.Sun.SunInternals" SunInternals.tests
  , testGroup "Data.Astro.Planet.PlanetDetails" PlanetDetails.tests
  , testGroup "Data.Astro.Planet.PlanetMechanics" PlanetMechanics.tests
  , testGroup "Data.Astro.Moon" Moon.tests
  ]
