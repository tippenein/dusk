module Main where

import App.Route
import Import
import Prelude

import Component.Event (Query(..), ui)
import Control.Coroutine (($$))
import Control.Coroutine as CR
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax as AX


main :: Eff (HA.HalogenEffects (ajax :: AX.AJAX)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI ui unit body
  io.query $ H.action $ GetEventList

