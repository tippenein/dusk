module Main where

import Import

import App.Router as Router
import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Types (HTMLElement)
import DOM.HTML.Window (document)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Top.Monad (TopEffects)

-- ui' :: H.Component Html Query Unit Void (Aff TopEffects)
ui' = H.hoist runTop Router.ui

main :: Eff TopEffects Unit
main = HA.runHalogenAff do
  io <- runUI ui' unit =<< awaitBody'
  void $ forkAff $ Router.routeSignal io

awaitBody' :: Aff TopEffects HTMLElement
awaitBody' = do
  body <- liftEff $ window >>= document >>= body
  maybe HA.awaitBody pure body
