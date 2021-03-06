module Data.Astro.CelestialObject.RiseSetTest
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

import Control.Monad (unless)

import Data.Astro.Types (DecimalDegrees(..), DecimalHours(..), fromDMS, fromHMS)
import Data.Astro.Coordinate (EquatorialCoordinates1(..))
import Data.Astro.Time.Sidereal (lstToDH, dhToLST)
import Data.Astro.CelestialObject.RiseSet
import Data.Astro.Star

tests = [testGroup "riseAndSet" [
             testRiseAndSet "a star, NH"
                 0.000001
                 (RiseSet (dhToLST 16.721731, DD 64.362370) (dhToLST 6.589380, DD 295.637630))
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
                 (RiseSet (dhToLST 2.507612, DD 120.857651) (dhToLST 10.997344, DD 239.142349))
                 (riseAndSet ecSirius verticalShift (DD 57))
             , testRiseAndSet "Sirius, SH"
                 0.000001
                 (RiseSet (dhToLST 23.390844, DD 116.158877) (dhToLST 14.114111, DD 243.841123))
                 (riseAndSet ecSirius verticalShift (DD $ -48))
             ]
        ]


ecPolaris = starCoordinates Polaris
ecAlphaCrucis = starCoordinates AlphaCrucis
ecSirius = starCoordinates Sirius

verticalShift = (fromDMS 0 34 0)

testRiseAndSet msg eps expected actual =
  testCase msg $ assertRiseAndSet eps expected actual

assertRiseAndSet eps expected@(RiseSet (etr, DD ear) (ets, DD eas)) actual@(RiseSet (atr, DD aar) (ats, DD aas)) =
  unless (eqLST eps etr atr && abs(ear-aar) <= eps
          && eqLST eps ets ats && abs(eas-aas) <= eps) (assertFailure msg)
  where msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual ++
              "\n (maximum margin of error: " ++ show eps ++ ")"
assertRiseAndSet _ Circumpolar Circumpolar = assertString ""
assertRiseAndSet _ Circumpolar actual = assertString msg
  where msg = "expected: Circumpolar\n but got: " ++ show actual
assertRiseAndSet _ NeverRises NeverRises = assertString ""
assertRiseAndSet _ NeverRises actual = assertString msg
  where msg = "expected: NeverRises\n but got: " ++ show actual

eqLST eps lst1 lst2 =
  let DH dh1 = lstToDH lst1
      DH dh2 = lstToDH lst2
  in abs (dh1 - dh2) < eps
