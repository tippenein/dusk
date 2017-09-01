module Helper.Format where

import Import

import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Formatter.DateTime as FD


prettyDateTime :: String -> Maybe DateTime
prettyDateTime x = hush $ FD.unformatDateTime "AA, MMM D" x

formatDateTime :: DateTime.DateTime -> Maybe String
formatDateTime x = hush $ FD.formatDateTime dtformat x

dtformat :: String
dtformat = "YYYY-MM-DD HH:mm"
-- dtformat = "YYYY-MM-DDTHH:mmZ"

unformatDateTime :: String -> Maybe DateTime.DateTime
unformatDateTime x = hush $ FD.unformatDateTime dtformat x

formatDate :: DateTime.Date -> Maybe String
formatDate x = formatDateTime $ DateTime.DateTime x bottom

unformatDate :: String -> Maybe DateTime.Date
unformatDate x = map DateTime.date $ unformatDateTime x

formatTime :: DateTime.Time -> Maybe String
formatTime x = hush $ FD.formatDateTime "HH:mm:ss.SSS" $ DateTime.DateTime bottom x

unformatTime :: String -> Maybe DateTime.Time
unformatTime x = hush $ map DateTime.time $ FD.unformatDateTime "HH:mm:ss.SSS" x
