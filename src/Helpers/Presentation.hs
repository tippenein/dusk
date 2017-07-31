module Helpers.Presentation where

import Import.NoFoundation

import Data.Time.Format (formatTime, defaultTimeLocale)

-- %A - long dotw
-- %B - long month name
-- https://hackage.haskell.org/package/time-1.8.0.2/docs/Data-Time-Format.html
showTime :: UTCTime -> Text
showTime = pack . formatTime defaultTimeLocale "%A, %B %d"

iso8601 :: UTCTime -> Text
iso8601 = pack . formatTime defaultTimeLocale "%FT%T%QZ"
