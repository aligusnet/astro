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

import Data.Astro.Moon (moonPosition1)
import Data.Astro.Moon.MoonDetails (j2010MoonDetails)

import Data.Astro.Planet (Planet(..), planetPosition, planetTrueAnomaly1)
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
calculateSunResult :: Params -> RiseSetResult
calculateSunResult params = case r of
  RiseSet rise set -> RiseSetResult { rise = lctToZonedTime <$> fst <$> rise
                                    , set = lctToZonedTime <$> fst <$> set
                                    , state = "Rise and/or set"}
  Circumpolar -> RiseSetResult Nothing Nothing "Circumpolar"
  NeverRises -> RiseSetResult Nothing Nothing "NeverRises"
  where coords = paramsCoordinates params
        date = paramsDate params
        r = sunRiseAndSet coords 0.833333 date


calculateMoonResult :: Params -> RiseSetResult
calculateMoonResult params = toRiseSetResult rs
  where position = moonPosition1 j2010MoonDetails
        coords = paramsCoordinates params
        verticalShift = refract (DD 0) 12 1012
        date = paramsDate params
        rs :: RiseSetMB
        rs = riseAndSet2 0.000001 position coords verticalShift date


calculatePlanetResult :: Params -> Planet -> RiseSetResult
calculatePlanetResult params planet = toRiseSetResult rs
  where coords = paramsCoordinates params
        verticalShift = refract (DD 0) 12 1012
        date = paramsDate params
        planetDetails = j2010PlanetDetails planet
        earthDetails = j2010PlanetDetails Earth
        position = planetPosition planetTrueAnomaly1 planetDetails earthDetails
        rs = riseAndSet2 0.000001 position coords verticalShift date


toRiseSetResult :: RiseSetMB -> RiseSetResult
toRiseSetResult rs = case rs of
  RiseSet rise set -> RiseSetResult { rise = lctToZonedTime <$> fst <$> rise
                                    , set = lctToZonedTime <$> fst <$> set
                                    , state = "Rise and/or set"}
  Circumpolar -> RiseSetResult Nothing Nothing "Circumpolar"
  NeverRises -> RiseSetResult Nothing Nothing "NeverRises"


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
data RiseSetResult = RiseSetResult {
  rise :: Maybe ZonedTime
  , set :: Maybe ZonedTime
  , state :: String
  } deriving (Generic, Show)

instance ToJSON RiseSetResult

data AstroResult = AstroResult {
  sun :: RiseSetResult
  , moon :: RiseSetResult
  , mercury :: RiseSetResult
  , venus :: RiseSetResult
  , mars :: RiseSetResult
  , jupiter :: RiseSetResult
  , saturn :: RiseSetResult
  , uranus :: RiseSetResult
  , neptune :: RiseSetResult
  } deriving (Generic, Show)

instance ToJSON AstroResult
