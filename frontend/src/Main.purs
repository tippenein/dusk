module Main where

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax as AX
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (forkAff)

import App.Router as Router
import Import


main :: Eff (HA.HalogenEffects (ajax :: AX.AJAX)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI Router.ui unit body
  forkAff $ Router.routeSignal io
