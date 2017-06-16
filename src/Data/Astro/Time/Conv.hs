{-|
Module: Data.Astro.Time.Conv
Description: Julian Date
Copyright: Alexander Ignatyev, 2017


Conversion functions between datetime types defined in Data.Time and Data.Astro.Time modules.
-}
module Data.Astro.Time.Conv
(
  zonedTimeToLCT
  , zonedTimeToLCD
  , lctToZonedTime
)

where


import Data.Time.LocalTime (ZonedTime(..), LocalTime(..)
                           , TimeOfDay(..), TimeZone(..)
                           , minutesToTimeZone)
import Data.Time.Calendar (toGregorian, fromGregorian)

import Data.Astro.Types(DecimalHours(..))
import Data.Astro.Utils (fromFixed)
import Data.Astro.Time.JulianDate (JulianDate(..)
                                  , LocalCivilTime(..)
                                  , LocalCivilDate(..)
                                  , fromYMDHMS, toYMDHMS
                                  , lctFromYMDHMS, lcdFromYMD
                                  , lctToYMDHMS)


-----------------------------------------------------------
-- Data.Time types -> Data.Astro types
timeZoneToDH :: TimeZone -> DecimalHours
timeZoneToDH  tz = DH hours
  where toMinutes = fromIntegral . timeZoneMinutes
        hours = (toMinutes tz) / 60.0


-- | Convert ZonedTime to LocalCivilTime
zonedTimeToLCT :: ZonedTime -> LocalCivilTime
zonedTimeToLCT zonedTime = lctFromYMDHMS tz y m d hours mins (fromFixed secs)
  where tz = timeZoneToDH (zonedTimeZone zonedTime)
        lt = zonedTimeToLocalTime zonedTime
        (y, m, d) = toGregorian (localDay lt)
        TimeOfDay hours mins secs = localTimeOfDay lt


-- | Convert ZonedTime to LocalCivilDate
zonedTimeToLCD :: ZonedTime -> LocalCivilDate
zonedTimeToLCD zonedTime = lcdFromYMD tz y m d
  where tz = timeZoneToDH (zonedTimeZone zonedTime)
        lt = zonedTimeToLocalTime zonedTime
        (y, m, d) = toGregorian (localDay lt)


-----------------------------------------------------------
-- Data.Astro Types -> Data.Time types

dhToTimeZone :: DecimalHours -> TimeZone
dhToTimeZone (DH hours) = minutesToTimeZone minutes
  where minutes = round (60*hours)


-- | Convert LocalCivilTime to ZonedTime
lctToZonedTime :: LocalCivilTime -> ZonedTime
lctToZonedTime lct = ZonedTime { zonedTimeToLocalTime = lt, zonedTimeZone = tz }
  where tz = dhToTimeZone $ lctTimeZone lct
        (y, m, d, hours, mins, secs) = lctToYMDHMS lct
        day = fromGregorian y m d
        time = TimeOfDay hours mins (realToFrac secs)
        lt = LocalTime { localDay = day, localTimeOfDay = time }
