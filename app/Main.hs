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

import Data.Astro.Effects (refract, parallax)
import Data.Astro.CelestialObject.RiseSet(riseAndSetLCT, riseAndSet2, RiseSetMB(..), RiseSetLCT(..))

import Data.Astro.Sun

import Data.Astro.Star

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
  , angularSize = angularSize'
  , position = hcPosition
  }
  where coords = paramsCoordinates params
        date = paramsDate params
        lct = paramsDateTime params
        jd = lctUniversalTime lct
        distance = sunDistance jd
        angularSize = sunAngularSize jd
        DD angularSize' = angularSize
        refractShift = refract (DD 0) 12 1012
        verticalShift = refractShift + (0.5 * angularSize)
        rs = sunRiseAndSet coords verticalShift date
        riseSet = toRiseSetResult rs
        ec1 = sunPosition2 jd
        hcPosition = toHorizonCoordinatesResult coords jd ec1


kmToAU :: Double -> AstronomicalUnits
kmToAU km = AU (km / 149597870.700)


moonPosition :: Double -> GeographicCoordinates -> JulianDate -> EquatorialCoordinates1
moonPosition distance coords jd =
  let p = moonPosition1 j2010MoonDetails jd
  in parallax coords 20 (kmToAU distance) jd p


calculateMoonResult :: Params -> PlanetaiResult
calculateMoonResult params = PR {
  riseSet = riseSet
  , distance = DR distance "km"
  , angularSize = angularSize'
  , position = hcPosition
  }
  where coords = paramsCoordinates params
        date = paramsDate params
        lct = paramsDateTime params
        jd = lctUniversalTime lct
        mdu = moonDistance1 j2010MoonDetails jd
        distance = mduToKm mdu
        angularSize = moonAngularSize mdu
        DD angularSize' = angularSize
        refractShift = refract (DD 0) 12 1012
        verticalShift = refractShift + (0.5 * angularSize)
        position = moonPosition distance coords
        rs = riseAndSet2 0.000001 position coords verticalShift date
        riseSet = toRiseSetResult rs
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


calculateStarResult :: Params -> Star -> StarResult
calculateStarResult params star = SR {
  starRiseSet = riseSet
  , starPosition = hcPosition
  }
  where coords = paramsCoordinates params
        verticalShift = refract (DD 0) 12 1012
        date = paramsDate params
        lct = paramsDateTime params
        jd = lctUniversalTime lct
        ec1 = starCoordinates star
        rs = riseAndSetLCT coords date verticalShift ec1
        riseSet = fromRiseSetLCT rs
        hcPosition = toHorizonCoordinatesResult coords jd ec1


toRiseSetResult :: RiseSetMB -> RiseSetResult
toRiseSetResult rs = case rs of
  RiseSet rise set -> RSR { rise = lctToZonedTime <$> fst <$> rise
                          , riseAzimuth = ddValue <$> snd <$> rise
                          , set = lctToZonedTime <$> fst <$> set
                          , setAzimuth = ddValue <$> snd <$> set
                          , state = "Rise and/or set"
                          }
  Circumpolar -> RSR Nothing Nothing Nothing Nothing "Circumpolar"
  NeverRises -> RSR Nothing Nothing Nothing Nothing "NeverRises"


fromRiseSetLCT :: RiseSetLCT -> RiseSetResult
fromRiseSetLCT rs = case rs of
  RiseSet rise set -> RSR { rise = Just $ lctToZonedTime $ fst rise
                          , riseAzimuth = Just $ ddValue $ snd $ rise
                          , set = Just $ lctToZonedTime $ fst set
                          , setAzimuth = Just $ ddValue $ snd $ set
                          , state = "Rise and Set"
                          }
  Circumpolar -> RSR Nothing Nothing Nothing Nothing "Circumpolar"
  NeverRises -> RSR Nothing Nothing Nothing Nothing "NeverRises"


ddValue :: DecimalDegrees -> Double
ddValue (DD value) = value

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
  request = params
  , sun = calculateSunResult params
  , moon = calculateMoonResult params
  , mercury = calculatePlanetResult params Mercury
  , venus = calculatePlanetResult params Venus
  , mars = calculatePlanetResult params Mars
  , jupiter = calculatePlanetResult params Jupiter
  , saturn = calculatePlanetResult params Saturn
  , uranus = calculatePlanetResult params Uranus
  , neptune = calculatePlanetResult params Neptune
  , polaris = calculateStarResult params Polaris
  , alphaCrucis = calculateStarResult params AlphaCrucis
  , sirius = calculateStarResult params Sirius
  , betelgeuse = calculateStarResult params Betelgeuse
  , rigel = calculateStarResult params Rigel
  , vega = calculateStarResult params Vega
  , antares = calculateStarResult params Antares
  , canopus = calculateStarResult params Canopus
  , pleiades = calculateStarResult params Pleiades
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
  , riseAzimuth :: Maybe Double
  , set :: Maybe ZonedTime
  , setAzimuth :: Maybe Double
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

data StarResult = SR {
  starRiseSet :: RiseSetResult
  , starPosition :: HorizonCoordinatesResult
  } deriving (Generic, Show)

instance ToJSON StarResult

data AstroResult = AstroResult {
  request :: Params
  , sun :: PlanetaiResult
  , moon :: PlanetaiResult
  , mercury :: PlanetaiResult
  , venus :: PlanetaiResult
  , mars :: PlanetaiResult
  , jupiter :: PlanetaiResult
  , saturn :: PlanetaiResult
  , uranus :: PlanetaiResult
  , neptune :: PlanetaiResult
  , polaris :: StarResult
  , alphaCrucis :: StarResult
  , sirius :: StarResult
  , betelgeuse :: StarResult
  , rigel :: StarResult
  , vega :: StarResult
  , antares :: StarResult
  , canopus :: StarResult
  , pleiades :: StarResult
  } deriving (Generic, Show)

instance ToJSON AstroResult
