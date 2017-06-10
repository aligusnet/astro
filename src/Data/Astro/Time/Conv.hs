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
                                  , fromYMDHMS, toYMDHMS)


-----------------------------------------------------------
-- Data.Time types -> Data.Astro types
timeZoneToDH :: TimeZone -> DecimalHours
timeZoneToDH  tz = DH hours
  where toMinutes = fromIntegral . timeZoneMinutes
        hours = (toMinutes tz) / 60.0


localTimeToJulianDate :: LocalTime -> JulianDate
localTimeToJulianDate lt =
  let (y, m, d) = toGregorian (localDay lt)
      TimeOfDay hours mins secs = localTimeOfDay lt
  in fromYMDHMS y m d hours mins (fromFixed secs)


-- | Convert ZonedTime to LocalCivilTime
zonedTimeToLCT :: ZonedTime -> LocalCivilTime
zonedTimeToLCT zonedTime = LCT { lctTimeZone = tz, lctUniversalTime = jd }
  where tz = timeZoneToDH (zonedTimeZone zonedTime)
        jd = localTimeToJulianDate $ zonedTimeToLocalTime zonedTime


-- | Convert ZonedTime to LocalCivilDate
zonedTimeToLCD :: ZonedTime -> LocalCivilDate
zonedTimeToLCD zonedTime = LCD { lcdTimeZone = tz, lcdDate = jd }
  where tz = timeZoneToDH (zonedTimeZone zonedTime)
        jd = localTimeToJulianDate $ zonedTimeToLocalTime zonedTime


-----------------------------------------------------------
-- Data.Astro Types -> Data.Time types

dhToTimeZone :: DecimalHours -> TimeZone
dhToTimeZone (DH hours) = minutesToTimeZone minutes
  where minutes = round (60*hours)


julianDateToLocalTime :: JulianDate -> LocalTime
julianDateToLocalTime jd = LocalTime { localDay = day, localTimeOfDay = time }
  where (y, m, d, hours, mins, secs) = toYMDHMS jd
        day = fromGregorian y m d
        time = TimeOfDay hours mins (realToFrac secs)


-- | Convert LocalCivilTime to ZonedTime
lctToZonedTime :: LocalCivilTime -> ZonedTime
lctToZonedTime lcd = ZonedTime { zonedTimeToLocalTime = lt, zonedTimeZone = tz }
  where tz = dhToTimeZone $ lctTimeZone lcd
        lt = julianDateToLocalTime $ lctUniversalTime lcd
