module Helper.Form where

import Prelude

import Control.Monad.Eff (Eff)
import Helper.Flatpicker as Flatpicker
import Top.Monad (TopEffects)

flatpicker :: String -> Eff TopEffects Unit
flatpicker s = Flatpicker.flatpicker(s)
