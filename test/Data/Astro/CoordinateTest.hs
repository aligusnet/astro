module Data.Astro.CoordinateTest
(
  tests
  , testEC1
  , testEcC
)

where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.HUnit.Approx
import Test.QuickCheck

import Control.Monad (unless)

import Data.Astro.TypesTest (testDecimalDegrees, testDecimalHours)
import Data.Astro.Time.JulianDate (JulianDate(..), LocalCivilTime(..), fromYMDHMS)
import Data.Astro.Coordinate
import Data.Astro.Types
import Data.Astro.Utils (reduceToZeroRange)

tests = [testGroup "RA <-> HA" [
            testDecimalHours "RA 18.53 in 1980-04-22 18:36:51.67 to HA"
                 0.0000001
                 (DH 9.873237)
                 (raToHA (DH 18.539167) (DD (-64.0)) $ fromYMDHMS 1980 4 22 18 36 51.67)
             , testDecimalHours "HA 9.87 in 1980-04-22 18:36:51.67 to RA"
                 0.0000001
                 (DH 9.873237)
                 (raToHA (DH 18.539167) (DD (-64.0)) $ fromYMDHMS 1980 4 22 18 36 51.67)
             , testProperty "RA <-> HA property for Novosibirsk" $ prop_HARAConv (DD 83) (JD 2457607.97281)
             , testProperty "RA <-> HA property for Rio de Janeiro" $ prop_HARAConv (DD (-43)) (JD 2457607.97281)
             ]
         , testGroup "EC <-> HC" [
             testHC "EC2 23.219 5.862 -> HC 19.334 283.271"
                 0.00000001
                 (HC (DD 19.334345224) (DD 283.27102726))
                 (equatorialToHorizon (DD 52) (EC2 (DD 23.219444444) (DH 5.862222222222222)))
             , testEC2 "HC 19.334 283.271 -> EC2 23.219 5.862"
                 0.00000001
                 (EC2 (DD 23.219444444) (DH 5.862222222222222))
                 (horizonToEquatorial (DD 52) (HC (DD 19.334345224) (DD 283.27102726)))
             , testProperty "property" prop_EC2HCConv
             ]
           , testGroup "obliquity" [
               testDecimalDegrees "2009-07-06"
                   0.000001
                   (DD 23.43925285)
                   (obliquity $ JD 2455018.5)
               , testDecimalDegrees "2016-08-12"
                   0.000001
                   (DD 23.434706359)
                   (obliquity $ JD 2457612.5)
               ]
           , testGroup "Ecliptic <-> Equatorial" [
               testEC1 "EcC 4.875 139.6861 on 2009-07-06 to EC1"
                   0.000001
                   (EC1 (DD 19.535712) (DH 9.581501))
                   (eclipticToEquatorial (EcC (DD 4.875277777777778) (DD 139.6861111111111)) (JD 2455018.5))
               , testEcC "EC1 19.535 9.581 to EcC on 2009-07-06"
                   0.000001
                   (EcC (DD 4.875278) (DD 139.686111))
                   (equatorialToEcliptic (EC1 (DD 19.535712) (DH 9.581501)) (JD 2455018.5))
               , testProperty "property" prop_EC1EcCConv
               ]
           , testGroup "Galactic <-> Equatorial" [
               testGC "EC 10.053 10.35 -> GC"
                   0.000001
                   (GC (DD 51.12226779935766) (DD 232.24788348766026))
                   (equatorialToGalactic (EC1 (DD 10.053055555555556) (DH 10.35)))
               , testEC1 "GC 51.122 232.247 -> EC"
                   0.000001
                   (EC1 (DD 10.053055555555556) (DH 10.35))
                   (galacticToEquatorial (GC (DD 51.12226779935766) (DD 232.24788348766026)))
               , testProperty "property" prop_EC1GCConv
               ]
         ]



prop_HARAConv longitude ut dh =
  let dh' = reduceToZeroRange 24 dh
      DH ra = haToRA (raToHA (DH dh') longitude ut) longitude ut
      DH ha = raToHA (haToRA (DH dh') longitude ut) longitude ut
      eps = 0.00000001
  in abs (ra - dh') < eps && (ha - dh') < eps
  where types = (dh::Double)

testHC msg eps expected actual =
  testCase msg $ assertHC eps expected actual

assertHC eps expected@(HC (DD eAlt) (DD eAz)) actual@(HC (DD aAlt) (DD aAz)) =
  unless (abs(eAlt-aAlt) <= eps && abs(eAz-aAz) <= eps) (assertFailure msg)
  where msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual ++
              "\n (maximum margin of error: " ++ show eps ++ ")"

testEC2 msg eps expected actual =
  testCase msg $ assertEC2 eps expected actual

assertEC2 eps expected@(EC2 (DD eDec) (DH eHa)) actual@(EC2 (DD aDec) (DH aHa)) =
  unless (abs(eDec-aDec) <= eps && abs(eHa-aHa) <= eps) (assertFailure msg)
  where msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual ++
              "\n (maximum margin of error: " ++ show eps ++ ")"

prop_EC2HCConv (latitude, up, round) =
  let latitude' = DD $ reduceToZeroRange 180 latitude
      up' = (reduceToZeroRange 89.9 up) + 0.1
      round' = (reduceToZeroRange 359.9 round) + 0.1
      HC (DD up'') (DD round'') = equatorialToHorizon latitude' $ horizonToEquatorial latitude' (HC (DD up') (DD round'))
      eps = 0.00001
  in abs(up'-up'') < eps && abs(round'-round'') < eps
  where types = ((latitude, up, round)::(Double, Double, Double))


testEC1 msg eps expected actual =
  testCase msg $ assertEC1 eps expected actual

assertEC1 eps expected@(EC1 (DD e1) (DH e2)) actual@(EC1 (DD a1) (DH a2)) =
  unless (abs(e1-a1) <= eps && abs(e2-a2) <= eps) (assertFailure msg)
  where msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual ++
              "\n (maximum margin of error: " ++ show eps ++ ")"


testEcC msg eps expected actual =
  testCase msg $ assertEcC eps expected actual

assertEcC eps expected@(EcC (DD eLat) (DD eLon)) actual@(EcC (DD aLat) (DD aLon)) =
  unless (abs(eLat-aLat) <= eps && abs(eLon-aLon) <= eps) (assertFailure msg)
  where msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual ++
              "\n (maximum margin of error: " ++ show eps ++ ")"

prop_EC1EcCConv (jd, up, round) =
  let jd' = JD $ (reduceToZeroRange 10000  jd) + 2455018.5
      up' = (reduceToZeroRange 90 up)
      round' = (reduceToZeroRange 359 round)
      EcC (DD up'') (DD round'') = equatorialToEcliptic (eclipticToEquatorial (EcC (DD up') (DD round')) jd') jd'
      eps = 0.000001
  in abs(up'-up'') < eps && abs(round'-round'') < eps
  where types = ((jd, up, round)::(Double, Double, Double))


testGC msg eps expected actual =
  testCase msg $ assertGC eps expected actual

assertGC eps expected@(GC (DD eLat) (DD eLon)) actual@(GC (DD aLat) (DD aLon)) =
  unless (abs(eLat-aLat) <= eps && abs(eLon-aLon) <= eps) (assertFailure msg)
  where msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual ++
              "\n (maximum margin of error: " ++ show eps ++ ")"

prop_EC1GCConv (up, round) =
  let up' = (reduceToZeroRange 90 up)
      round' = (reduceToZeroRange 359 round)
      GC (DD up'') (DD round'') = equatorialToGalactic $ galacticToEquatorial (GC (DD up') (DD round'))
      eps = 0.000001
  in abs(up'-up'') < eps && abs(round'-round'') < eps
  where types = ((up, round)::(Double, Double))
