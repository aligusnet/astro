module Data.Astro.TypesTest
(
  tests
  , testDecimalDegrees
  , testDecimalHours
)

where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.HUnit.Approx
import Test.QuickCheck

import Data.Ratio ((%))
import Data.Astro.Types

tests = [testGroup "DecimalDegrees <-> DecimalHours" [
            testDecimalDegrees "12.03 H -> 180.45 D" 0.000001 (DD 180.45) $ fromDecimalHours (DH 12.03)
            , testDecimalHours "180.45 D -> 12.03 H" 0.000001 (DH 12.03) $ toDecimalHours (DD 180.45)
            , testProperty "property" prop_DHConversion
            ]
        , testGroup "DecimalDegrees <-> Radians" [
            testCase "0 -> 0 (rad)" $ assertApproxEqual "" 0.000000001 0 $ toRadians (DD 0)
            , testCase "45 -> PI/4" $ assertApproxEqual "" 0.000000001 (pi*0.25) $ toRadians (DD 45)
            , testCase "90 -> PI/2" $ assertApproxEqual "" 0.000000001 (pi*0.5) $ toRadians (DD 90)
            , testCase "180 -> PI" $ assertApproxEqual "" 0.000000001 pi $ toRadians (DD 180)
            , testCase "360 -> 2*PI" $ assertApproxEqual "" 0.000000001 (pi*2) $ toRadians (DD 360)
            , testDecimalDegrees "0 -> 0 (deg)" 0.000000001 (DD 0) (fromRadians 0)
            , testDecimalDegrees "pi/4 -> 45" 0.000000001 (DD 45) (fromRadians (pi*0.25))
            , testDecimalDegrees "pi/2 -> 90" 0.000000001 (DD 90) (fromRadians (pi*0.5))
            , testDecimalDegrees "pi -> 180" 0.000000001 (DD 180) (fromRadians pi)
            , testDecimalDegrees "2*pi -> 360" 0.000000001 (DD 360) (fromRadians (pi*2))
            ]
          , testGroup "DecimalDegrees <-> DMS" [
              testDecimalDegrees "182 31' 27''" 0.00001 (DD 182.52417) $ fromDMS 182 31 27
              , testCase "182.5" $ toDMS (DD 182.5) @?= (182, 30, 0)
              , testProperty "property" prop_DMSConversion
            ]
          , testGroup "DecimalHours <-> HMS" [
              testDecimalHours "HMS -> DH 6:00" 0.00000001 (DH 6.0) (fromHMS 6 0 0)
            , testDecimalHours "HMS -> DH 18:00" 0.00000001 (DH 18.0) (fromHMS 18 0 0)
            , testDecimalHours "HMS -> DH 18:30" 0.00000001 (DH $ (18*2 + 1) / 2) (fromHMS 18 30 0)
            , testDecimalHours "HMS -> DH 00:00:30" 0.00000001 (DH $ 30 / (60*60)) (fromHMS  0 0 30) 
            , testDecimalHours "HMS -> DH 00:00:10" 0.00000001 (DH 0.002777777778) $ fromHMS 0 0 10
            , testDecimalHours "HMS -> DH 23:59:59.99999" 0.00000001 (DH 24.0) $ fromHMS 23 59 59.99999
            , testCase "DH -> HMS 6:00" $ toHMS (DH 6.0)  @?= (6, 0, 0)
            , testCase "DH -> HMS 18:00" $ toHMS (DH 18.0) @?= (18, 0, 0)
            , testCase "DH -> HMS 18:30" $ toHMS  (DH $ (18*2 + 1) / 2) @?= (18, 30, 0)
            , testCase "DH -> HMS 00:00:30" $ toHMS (DH $ (30 / (60*60))) @?= (0, 0, 30)
            , testProperty "property" prop_HMSConversion 
            ]
          , testGroup "Light travel time" [
              testDecimalHours "7.7 AU" 0.0000001 1.06722 (lightTravelTime 7.7)
              ]
          , testGroup "KM <-> AU" [
              testCase "KM -> AU" $ assertApproxEqual "" 1e-5 (AU 7.8) (kmToAU 1166863391.46)
              , testCase "AU -> KM" $ assertApproxEqual "" 1e-5 1166863391.46 (auToKM 7.8)
              ]
          , testGroup "DD: standard typeclasses" [
              testCase "show" $ "DD 15.5" @=? show (DD 15.5)
              , testCase "showList" $ "[DD 15.3,DD 15.7]" @=? showList [DD 15.3, DD 15.7] ""
              , testCase "showsPrec" $ "DD 15.5" @=? showsPrec 0 (DD 15.5) ""
              , testCase "== (True)" $ True @=? (DD 15.5) == (DD 15.5)
              , testCase "== (False)" $ False @=? (DD 15.3) == (DD 15.5)
              , testCase "/= (True)" $ True @=? (DD 15.3) /= (DD 15.5)
              , testCase "/= (False)" $ False @=? (DD 15.5) /= (DD 15.5)
              , testCase "compare: LT" $ LT @=? (DD 15.3) `compare` (DD 15.5)
              , testCase "compare: EQ" $ EQ @=? (DD 15.5) `compare` (DD 15.5)
              , testCase "compare: GT" $ GT @=? (DD 15.7) `compare` (DD 15.5)
              , testCase "<" $ True @=? (DD 15.3) < (DD 15.7)
              , testCase "<=" $ True @=? (DD 15.3) <= (DD 15.7)
              , testCase ">" $ False @=? (DD 15.3) > (DD 15.7)
              , testCase ">=" $ False @=? (DD 15.3) >= (DD 15.7)
              , testCase "max" $ (DD 15.7) @=? max (DD 15.3) (DD 15.7)
              , testCase "min" $ (DD 15.3) @=? min (DD 15.3) (DD 15.7)
              , testCase "abs" $ (DD 15.7) @=? abs (DD (-15.7))
              , testCase "signum > 0" $ (DD 1.0) @=? signum (DD 15.5)
              , testCase "signum = 0" $ (DD 0.0) @=? signum (DD 0.0)
              , testCase "signum < 0" $ (DD $ -1.0) @=? signum (DD $ -15.5)
              , testCase "toRational" $ (31 % 2) @=? toRational (DD 15.5)
              , testCase "recip" $ (DD 0.01) @=? recip (DD 100)
              , testCase "properFraction" $ (15, DD 0.5) @=? properFraction (DD 15.5)
              ]
          , testGroup "DH: standard typeclasses" [
              testCase "show" $ "DH 15.5" @=? show (DH 15.5)
              , testCase "showList" $ "[DH 15.3,DH 15.7]" @=? showList [DH 15.3, DH 15.7] ""
              , testCase "showsPrec" $ "DH 15.5" @=? showsPrec 0 (DH 15.5) ""
              , testCase "== (True)" $ True @=? (DH 15.5) == (DH 15.5)
              , testCase "== (False)" $ False @=? (DH 15.3) == (DH 15.5)
              , testCase "/= (True)" $ True @=? (DH 15.3) /= (DH 15.5)
              , testCase "/= (False)" $ False @=? (DH 15.5) /= (DH 15.5)
              , testCase "compare: LT" $ LT @=? (DH 15.3) `compare` (DH 15.5)
              , testCase "compare: EQ" $ EQ @=? (DH 15.5) `compare` (DH 15.5)
              , testCase "compare: GT" $ GT @=? (DH 15.7) `compare` (DH 15.5)
              , testCase "<" $ True @=? (DH 15.3) < (DH 15.7)
              , testCase "<=" $ True @=? (DH 15.3) <= (DH 15.7)
              , testCase ">" $ False @=? (DH 15.3) > (DH 15.7)
              , testCase ">=" $ False @=? (DH 15.3) >= (DH 15.7)
              , testCase "max" $ (DH 15.7) @=? max (DH 15.3) (DH 15.7)
              , testCase "min" $ (DH 15.3) @=? min (DH 15.3) (DH 15.7)
              , testCase "abs" $ (DH 15.7) @=? abs (DH (-15.7))
              , testCase "signum > 0" $ (DH 1.0) @=? signum (DH 15.5)
              , testCase "signum = 0" $ (DH 0.0) @=? signum (DH 0.0)
              , testCase "signum < 0" $ (DH $ -1.0) @=? signum (DH $ -15.5)
              , testCase "toRational" $ (31 % 2) @=? toRational (DH 15.5)
              , testCase "recip" $ (DH 0.01) @=? recip (DH 100)
              , testCase "properFraction" $ (15, DH 0.5) @=? properFraction (DH 15.5)
              ]
          , testGroup "AU: standard typeclasses" [
              testCase "show" $ "AU 15.5" @=? show (AU 15.5)
              , testCase "showList" $ "[AU 15.3,AU 15.7]" @=? showList [AU 15.3, AU 15.7] ""
              , testCase "showsPrec" $ "AU 15.5" @=? showsPrec 0 (AU 15.5) ""
              , testCase "== (True)" $ True @=? (AU 15.5) == (AU 15.5)
              , testCase "== (False)" $ False @=? (AU 15.3) == (AU 15.5)
              , testCase "/= (True)" $ True @=? (AU 15.3) /= (AU 15.5)
              , testCase "/= (False)" $ False @=? (AU 15.5) /= (AU 15.5)
              , testCase "compare: LT" $ LT @=? (AU 15.3) `compare` (AU 15.5)
              , testCase "compare: EQ" $ EQ @=? (AU 15.5) `compare` (AU 15.5)
              , testCase "compare: GT" $ GT @=? (AU 15.7) `compare` (AU 15.5)
              , testCase "<" $ True @=? (AU 15.3) < (AU 15.7)
              , testCase "<=" $ True @=? (AU 15.3) <= (AU 15.7)
              , testCase ">" $ False @=? (AU 15.3) > (AU 15.7)
              , testCase ">=" $ False @=? (AU 15.3) >= (AU 15.7)
              , testCase "max" $ (AU 15.7) @=? max (AU 15.3) (AU 15.7)
              , testCase "min" $ (AU 15.3) @=? min (AU 15.3) (AU 15.7)
              , testCase "+" $ (AU 17.5) @=? (AU 15.5) + (AU 2)
              , testCase "-" $ (AU 13.5) @=? (AU 15.5) - (AU 2)
              , testCase "*" $ (AU 31) @=? (AU 15.5) * (AU 2)
              , testCase "negate" $ (AU 15.5) @=? negate (AU $ -15.5)
              , testCase "abs" $ (AU 15.7) @=? abs (AU (-15.7))
              , testCase "signum > 0" $ (AU 1.0) @=? signum (AU 15.5)
              , testCase "signum = 0" $ (AU 0.0) @=? signum (AU 0.0)
              , testCase "signum < 0" $ (AU $ -1.0) @=? signum (AU $ -15.5)
              , testCase "fromInteger" $ (AU 17) @=? fromInteger 17
              , testCase "toRational" $ (31 % 2) @=? toRational (AU 15.5)
              , testCase "/" $ (AU 10) @=? (AU 30) / (AU 3)
              , testCase "recip" $ (AU 0.01) @=? recip (AU 100)
              , testCase "properFraction" $ (15, AU 0.5) @=? properFraction (AU 15.5)
              ]
        ]


testDecimalDegrees msg eps (DD expected) (DD actual) =
  testCase msg $ assertApproxEqual "" eps expected actual

testDecimalHours msg eps (DH expected) (DH actual) =
  testCase msg $ assertApproxEqual "" eps expected actual

prop_DHConversion n =
  let DH h = toDecimalHours . fromDecimalHours $ DH n
      DD d = fromDecimalHours . toDecimalHours $ DD n
      eps = 0.00000001
  in abs(n-h) < eps && abs(n-d) < eps
  where types = (n::Double)      

prop_DMSConversion dd =
  let (d, m, s) = toDMS $ DD dd
      DD d' = fromDMS d m s
  in abs(dd-d') < 0.0000001
  where types = (dd::Double)

prop_HMSConversion =
  forAll (choose (0, 1.0)) $ checkHMSConversionProperties

checkHMSConversionProperties :: Double -> Bool
checkHMSConversionProperties n =
  let (h, m, s) = toHMS $ DH n
      DH n2 = fromHMS h m s
  in abs (n-n2) < 0.00000001
