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
