module Data.Astro.CelestialObjectTest
(
  tests
)

where

  
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.HUnit.Approx
import Test.QuickCheck

import Data.Astro.TypesTest (testDecimalDegrees)

import Control.Monad (unless)

import Data.Astro.Types (DecimalDegrees(..), DecimalHours(..), fromDMS, fromHMS)
import Data.Astro.Coordinate (EquatorialCoordinates1(..), EclipticCoordinates(..))
import Data.Astro.CelestialObject

tests = [testGroup "angle" [
            testDecimalDegrees "Equatorial: Orionis and Canis Majoris"
                0.000000001
                (DD 23.673849422164192)
                (angleEquatorial (EC1 (DD (-8.225)) (DH 5.225472222222222)) (EC1 (DD (-16.68638888888889)) (DH 6.737055555555555)))
            , testDecimalDegrees "Ecliptic: Orionis and Canis Majoris"
                0.000000001
                (DD 23.673849422164192)
                (angleEcliptic (EcC (DD (-31.12290508933333)) (DD 76.53651836408739)) (EcC (DD (-39.597832824969665)) (DD 103.79168740150627)))
            ]
         , testGroup "riseAndSet" [
             testRiseAndSet "a star, NH"
                 0.000001
                 (RiseSet (DH 16.721731, DD 64.362370) (DH 6.589380, DD 295.637630))
                 (riseAndSet (EC1 (fromDMS 21 42 0) (fromHMS 23 39 20)) verticalShift (DD 30))
             , testRiseAndSet "Polaris, NH"
                 0.000001
                 Circumpolar
                 (riseAndSet ecPolaris verticalShift (DD 30))
             , testRiseAndSet "Polaris, SH"
                 0.000001
                 NeverRises
                 (riseAndSet ecPolaris verticalShift (DD $ -30))
             , testRiseAndSet "Alpha Crucis, NH"
                 0.000001
                 NeverRises
                 (riseAndSet ecAlphaCrucis verticalShift (DD 30))
             , testRiseAndSet "Alpha Crucis, SH"
                 0.000001
                 Circumpolar
                 (riseAndSet ecAlphaCrucis verticalShift (DD $ -30))
             , testRiseAndSet "Sirius, NH"
                 0.000001
                 (RiseSet (DH 2.507612, DD 120.857651) (DH 10.997344, DD 239.142349))
                 (riseAndSet ecSirius verticalShift (DD 57))
             , testRiseAndSet "Sirius, SH"
                 0.000001
                 (RiseSet (DH 23.390844, DD 116.158877) (DH 14.114111, DD 243.841123))
                 (riseAndSet ecSirius verticalShift (DD $ -48))
             ]
        ]


ecPolaris = EC1 (fromDMS 89 15 51) (fromHMS 2 31 48.7)
ecAlphaCrucis = EC1 (-(fromDMS 63 5 56.73)) (fromHMS 12 26 35.9)
ecSirius = EC1 (-(fromDMS 16 42 58.02)) (fromHMS 6 45 8.92)

verticalShift = (fromDMS 0 34 0)

testRiseAndSet msg eps expected actual =
  testCase msg $ assertRiseAndSet eps expected actual

assertRiseAndSet eps expected@(RiseSet (DH etr, DD ear) (DH ets, DD eas)) actual@(RiseSet (DH atr, DD aar) (DH ats, DD aas)) =
  unless (abs(etr-atr) <= eps && abs(ear-aar) <= eps
          && abs(ets-ats) <= eps && abs(eas-aas) <= eps) (assertFailure msg)
  where msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual ++
              "\n (maximum margin of error: " ++ show eps ++ ")"
assertRiseAndSet _ Circumpolar Circumpolar = assertString ""
assertRiseAndSet _ Circumpolar actual = assertString msg
  where msg = "expected: " ++ show Circumpolar ++ "\n but got: " ++ show actual
assertRiseAndSet _ NeverRises NeverRises = assertString ""
assertRiseAndSet _ NeverRises actual = assertString msg
  where msg = "expected: " ++ show NeverRises ++ "\n but got: " ++ show actual
