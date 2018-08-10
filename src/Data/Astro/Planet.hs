{-|
Module: Data.Astro.Planet
Description: Planet calculations
Copyright: Alexander Ignatyev, 2016

Planet calculations.

= Example

=== /Initialisation/

@
import Data.Astro.Time.JulianDate
import Data.Astro.Coordinate
import Data.Astro.Types
import Data.Astro.Effects
import Data.Astro.CelestialObject.RiseSet
import Data.Astro.Planet

ro :: GeographicCoordinates
ro = GeoC (fromDMS 51 28 40) (-(fromDMS 0 0 5))

dt :: LocalCivilTime
dt = lctFromYMDHMS (DH 1) 2017 6 25 10 29 0

today :: LocalCivilDate
today = lcdFromYMD (DH 1) 2017 6 25

jupiterDetails :: PlanetDetails
jupiterDetails = j2010PlanetDetails Jupiter

earthDetails :: PlanetDetails
earthDetails = j2010PlanetDetails Earth

jupiterPosition :: JulianDate -> EquatorialCoordinates1
jupiterPosition = planetPosition planetTrueAnomaly1 jupiterDetails earthDetails
@

=== /Calcaulate Coordinates/
@
jupiterEC1 :: EquatorialCoordinates1
jupiterEC1 = jupiterPosition (lctUniversalTime dt)
-- EC1 {e1Declination = DD (-4.104626810672402), e1RightAscension = DH 12.863365504382228}

jupiterHC :: HorizonCoordinates
jupiterHC = ec1ToHC ro (lctUniversalTime dt) jupiterEC1
-- HC {hAltitude = DD (-30.67914598469227), hAzimuth = DD 52.29376845044007}
@

=== /Calculate Distance/
@
jupiterDistance :: AstronomicalUnits
jupiterDistance = planetDistance1 jupiterDetails earthDetails (lctUniversalTime dt)
-- AU 5.193435872521039
@

=== /Calculate Angular Size/
@
jupiterAngularSize :: DecimalDegrees
jupiterAngularSize = planetAngularDiameter jupiterDetails jupiterDistance
-- DD 1.052289877865987e-2

toDMS jupiterAngularSize
-- (0,0,37.88243560317554)
@

=== /Calculate Rise and Set/

@
verticalShift :: DecimalDegrees
verticalShift = refract (DD 0) 12 1012
-- DD 0.5660098245614035

jupiterRiseSet :: RiseSetMB
jupiterRiseSet = riseAndSet2 0.000001 jupiterPosition ro verticalShift today
-- RiseSet
--    (Just (2017-06-25 13:53:27.3109 +1.0,DD 95.88943953535569))
--    (Just (2017-06-25 01:21:23.5835 +1.0,DD 264.1289033612776))
@
-}

module Data.Astro.Planet
(
  Details.Planet(..)
  , Details.PlanetDetails(..)
  , Details.j2010PlanetDetails
  , Mechanics.planetTrueAnomaly1
  , Mechanics.planetTrueAnomaly2
  , Mechanics.planetPosition
  , Mechanics.planetPosition1
  , Mechanics.planetDistance
  , Mechanics.planetDistance1
  , Mechanics.planetAngularDiameter
  , Mechanics.planetPhase1
  , Mechanics.planetBrightLimbPositionAngle
)

where


import qualified Data.Astro.Planet.PlanetDetails as Details
import qualified Data.Astro.Planet.PlanetMechanics as Mechanics
