{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (fromMaybe)
import Data.Time.LocalTime (ZonedTime, getZonedTime)
import Options.Applicative
import Data.Monoid((<>))

-- Astro Imports
import Data.Astro.Time.JulianDate
import Data.Astro.Time.Conv (zonedTimeToLCT, zonedTimeToLCD, lctToZonedTime)

import Data.Astro.Effects (refract)
import Data.Astro.CelestialObject.RiseSet(riseAndSet2, RiseSetMB(..))

import Data.Astro.Sun

import Data.Astro.Types
import Data.Astro.Coordinate

import Data.Astro.Moon (moonPosition1, moonDistance1, moonAngularSize)
import Data.Astro.Moon.MoonDetails (j2010MoonDetails, mduToKm)

import Data.Astro.Planet (Planet(..), planetPosition, planetTrueAnomaly1, planetDistance1, planetAngularDiameter)
import Data.Astro.Planet.PlanetDetails (j2010PlanetDetails)


main :: IO ()
main = execParser opts >>= run
  where opts = info (cmdOptions <**> helper)
          ( progDesc "Amateur astronomical computations"
            <> header "Astro" )


run :: CmdOptions -> IO ()
run cmdOptions = do
  defParams <- defaultParams
  let params = fromMaybe defParams $ fromMaybe defParams <$> decode <$> B.pack <$> cmdJson cmdOptions
      res = processQuery params
  B.putStrLn $ encode res


-- Calcs
calculateSunResult :: Params -> PlanetaiResult
calculateSunResult params = PR {
  riseSet = riseSet
  , distance = DR distance "km"
  , angularSize = angularSize
  , position = hcPosition
  }
  where coords = paramsCoordinates params
        date = paramsDate params
        lct = paramsDateTime params
        jd = lctUniversalTime lct
        rs = sunRiseAndSet coords 0.833333 date
        riseSet = toRiseSetResult rs
        distance = sunDistance jd
        DD angularSize = sunAngularSize jd
        ec1 = sunPosition2 jd
        hcPosition = toHorizonCoordinatesResult coords jd ec1


calculateMoonResult :: Params -> PlanetaiResult
calculateMoonResult params = PR {
  riseSet = riseSet
  , distance = DR distance "km"
  , angularSize = angularSize
  , position = hcPosition
  }
  where position = moonPosition1 j2010MoonDetails
        coords = paramsCoordinates params
        verticalShift = refract (DD 0) 12 1012
        date = paramsDate params
        lct = paramsDateTime params
        jd = lctUniversalTime lct
        rs = riseAndSet2 0.000001 position coords verticalShift date
        riseSet = toRiseSetResult rs
        mdu = moonDistance1 j2010MoonDetails jd
        distance = mduToKm mdu
        DD angularSize = moonAngularSize mdu
        ec1 = position jd
        hcPosition = toHorizonCoordinatesResult coords jd ec1


calculatePlanetResult :: Params -> Planet -> PlanetaiResult
calculatePlanetResult params planet = PR {
  riseSet = riseSet
  , distance = DR distance "AU"
  , angularSize = angularSize
  , position = hcPosition
  }
  where coords = paramsCoordinates params
        verticalShift = refract (DD 0) 12 1012
        date = paramsDate params
        lct = paramsDateTime params
        jd = lctUniversalTime lct
        planetDetails = j2010PlanetDetails planet
        earthDetails = j2010PlanetDetails Earth
        position = planetPosition planetTrueAnomaly1 planetDetails earthDetails
        rs = riseAndSet2 0.000001 position coords verticalShift date
        riseSet = toRiseSetResult rs
        au = planetDistance1 planetDetails earthDetails jd
        AU distance = au
        DD angularSize = planetAngularDiameter planetDetails au
        ec1 = position jd
        hcPosition = toHorizonCoordinatesResult coords jd ec1



toRiseSetResult :: RiseSetMB -> RiseSetResult
toRiseSetResult rs = case rs of
  RiseSet rise set -> RSR { rise = lctToZonedTime <$> fst <$> rise
                          , set = lctToZonedTime <$> fst <$> set
                          , state = "Rise and/or set"}
  Circumpolar -> RSR Nothing Nothing "Circumpolar"
  NeverRises -> RSR Nothing Nothing "NeverRises"


toHorizonCoordinatesResult :: GeographicCoordinates
                           -> JulianDate
                           -> EquatorialCoordinates1
                           -> HorizonCoordinatesResult
toHorizonCoordinatesResult (GeoC lat long) jd (EC1 delta alpha) = HCR altitude azimuth
  where ec2 = EC2 delta (raToHA alpha long jd)
        hc = equatorialToHorizon lat ec2
        HC (DD altitude) (DD azimuth) = hc
        
        
        

processQuery :: Params -> AstroResult
processQuery params = AstroResult {
  sun = calculateSunResult params
  , moon = calculateMoonResult params
  , mercury = calculatePlanetResult params Mercury
  , venus = calculatePlanetResult params Venus
  , mars = calculatePlanetResult params Mars
  , jupiter = calculatePlanetResult params Jupiter
  , saturn = calculatePlanetResult params Saturn
  , uranus = calculatePlanetResult params Uranus
  , neptune = calculatePlanetResult params Neptune
  }


-- Command Line Options
data CmdOptions = CmdOptions {
  cmdJson :: Maybe String
  }


cmdOptions :: Parser CmdOptions
cmdOptions = CmdOptions
  <$> (optional $ strOption ( long "json" <> short 'j' <> help "JSON-encoded params") )


-- Params
data CoordinatesParam = CoordinatesParam {
    latitude :: Double
  , longitude  :: Double
  } deriving (Generic, Show)

instance ToJSON CoordinatesParam
instance FromJSON CoordinatesParam


data Params = Params {
  coordinates :: CoordinatesParam
  , datetime :: ZonedTime
  } deriving (Generic, Show)

instance ToJSON Params
instance FromJSON Params


paramsCoordinates :: Params -> GeographicCoordinates
paramsCoordinates params = GeoC (DD $ latitude coords) (DD $ longitude coords)
  where coords = coordinates params


paramsDateTime :: Params -> LocalCivilTime
paramsDateTime = zonedTimeToLCT . datetime


paramsDate :: Params -> LocalCivilDate
paramsDate = zonedTimeToLCD . datetime


greenwichCoordinates :: CoordinatesParam
greenwichCoordinates = CoordinatesParam 51.4768 0


defaultParams :: IO (Params)
defaultParams = do
  time <- getZonedTime
  return Params {
    coordinates = greenwichCoordinates
    , datetime = time
    }


-- Result
data HorizonCoordinatesResult = HCR {
  altitude :: Double
  , azimuth :: Double
  } deriving (Generic, Show)

instance ToJSON HorizonCoordinatesResult

data RiseSetResult = RSR {
  rise :: Maybe ZonedTime
  , set :: Maybe ZonedTime
  , state :: String
  } deriving (Generic, Show)

instance ToJSON RiseSetResult


data DistanceResult = DR {
  value :: Double
  , units :: String
  } deriving (Generic, Show)

instance ToJSON DistanceResult

data PlanetaiResult = PR {
  riseSet :: RiseSetResult
  , distance :: DistanceResult
  , angularSize:: Double
  , position :: HorizonCoordinatesResult             
  } deriving (Generic, Show)

instance ToJSON PlanetaiResult

data AstroResult = AstroResult {
  sun :: PlanetaiResult
  , moon :: PlanetaiResult
  , mercury :: PlanetaiResult
  , venus :: PlanetaiResult
  , mars :: PlanetaiResult
  , jupiter :: PlanetaiResult
  , saturn :: PlanetaiResult
  , uranus :: PlanetaiResult
  , neptune :: PlanetaiResult
  } deriving (Generic, Show)

instance ToJSON AstroResult
