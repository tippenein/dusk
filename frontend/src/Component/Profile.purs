module Component.Profile where

import Routes as Routes
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Prelude (class Eq, class Ord, type (~>), Unit, Void, const, pure, unit)
import Top.Monad (Top)

data Input a
  = Noop a

type State = Unit

data Slot = Slot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

ui :: H.Component HH.HTML Input Unit Routes.ChildAction Top
ui = H.component
  { initialState: const unit
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render _ =
      HH.div_
        [ HH.h1_ [ HH.text "Your Profile" ]
        , HH.p_ [ HH.text "what a nice profile!" ]
        ]

    eval :: Input ~> H.ComponentDSL State Input Routes.ChildAction Top
    eval (Noop n) = pure n
