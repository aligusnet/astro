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

import Data.Astro.Sun
import Data.Astro.Types
import Data.Astro.Time.JulianDate
import Data.Astro.Time.Conv (zonedTimeToLCT, zonedTimeToLCD, lctToZonedTime)


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
calculateSunResult :: Params -> SunResult
calculateSunResult params = case r of
  RiseSet rise set -> SunResult { rise = lctToZonedTime <$> fst <$> rise, set = lctToZonedTime <$> fst <$> set }
  _ -> SunResult Nothing Nothing 
  where coords = paramsCoordinates params
        date = paramsDate params
        r = sunRiseAndSet coords 0.833333 date

processQuery :: Params -> AstroResult
processQuery params = AstroResult {
  sun = calculateSunResult params
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
data SunResult = SunResult {
  rise :: Maybe ZonedTime
  , set :: Maybe ZonedTime
  } deriving (Generic, Show)

instance ToJSON SunResult

data AstroResult = AstroResult {
  sun :: SunResult
  } deriving (Generic, Show)

instance ToJSON AstroResult
