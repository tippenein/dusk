module Component.Admin.Main where


import Helper

import Component.Admin.Curator as Curator
import Component.Admin.Event as Event
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.DateTime as DateTime
import Data.Either.Nested (Either2)
import Data.Formatter.DateTime as FD
import Data.Functor.Coproduct.Nested (Coproduct2)
import Halogen as H
import Halogen.Component.ChildPath (ChildPath(..), cp1, cp2)
import Halogen.HTML hiding (map)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Import hiding (div)
import Network.HTTP.Affjax as AX


data Slot = Slot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type State =
  { loading :: Boolean
  , error :: Maybe String
  }

data Input a
  = Noop a

type ChildQuery = Coproduct2 Curator.Input Event.Input
type ChildSlot = Either2 Curator.Slot Event.Slot

pathToCurators :: ChildPath Curator.Input ChildQuery Curator.Slot ChildSlot
pathToCurators = cp1

pathToEvents :: ChildPath Event.Input ChildQuery Event.Slot ChildSlot
pathToEvents = cp2

ui :: H.Component HTML Input Unit Void Top
ui =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState = { loading: false, error: Nothing }

  eval :: Input ~> H.ParentDSL State Input ChildQuery ChildSlot Void Top
  eval = case _ of
    Noop next -> pure next

  render st =
    div [ styleClass "container admin-page" ]
      [ div [ styleClass "row" ]
        [ div [ styleClass "col-lg-6" ]
          [ slot' pathToCurators Curator.Slot Curator.ui unit absurd ]
        , div [ styleClass "col-lg-6" ]
          [ slot' pathToEvents Event.Slot Event.ui unit absurd
          ]
        ]
      ]
