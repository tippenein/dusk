module Helper.Presentation where

import Import.NoFoundation

import Data.Time.Format (formatTime, defaultTimeLocale)

-- %A - long dotw
-- %B - long month name
-- https://hackage.haskell.org/package/time-1.8.0.2/docs/Data-Time-Format.html
showTime :: UTCTime -> Text
showTime = pack . formatTime defaultTimeLocale "%A, %B %d"

iso8601 :: UTCTime -> Text
iso8601 = pack . formatTime defaultTimeLocale "%FT%T%QZ"

lorem :: Text
lorem = "Nos istuc amplitudines fuisse consuetudinis tamdiu manu instat illic, mira motus, atque e alibi caste huc id commendavi auris. Scirent subditus turibulis vult fit interrogare en. Dum multiplices dicere hae latis a habiti valida quaerit palliata ipso ambiendum ex os ne nam tam eo impium tuus dinoscens."

cycle :: [a] -> [a]
cycle [] = []
cycle xs = xs' where xs' = xs ++ xs'

lorems :: Int -> [Text]
lorems n = take n $ cycle [a,b,c]
  where
    a = "Nos istuc amplitudines fuisse consuetudinis tamdiu manu instat illic, mira motus, atque e alibi caste huc id commendavi auris."
    b = "Scirent subditus turibulis vult fit interrogare en."
    c = "Dum multiplices dicere hae latis a habiti valida quaerit palliata ipso ambiendum ex os ne nam tam eo impium tuus dinoscens."
