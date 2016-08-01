import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.Astro.CalendarTest

main = defaultMain tests

tests = [testGroup "Data.Astro.Calendar" Data.Astro.CalendarTest.tests
        ]
