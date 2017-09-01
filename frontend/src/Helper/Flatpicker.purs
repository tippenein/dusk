module Helper.Flatpicker where

import Control.Monad.Eff

import DOM (DOM)
import Top.Monad (TopEffects)
import Prelude (Unit)


foreign import flatpicker :: String -> Eff TopEffects Unit
