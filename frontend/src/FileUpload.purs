module FileUpload where

import Control.Monad.Eff
import Prelude

import Control.Monad.Aff (Aff)
import Network.HTTP.Affjax as AX
import Top.Monad (TopEffects)

foreign import fileUpload :: forall eff. String -> String -> Eff ( ajax :: AX.AJAX | eff) Unit
