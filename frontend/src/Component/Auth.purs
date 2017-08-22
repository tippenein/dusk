module Component.Auth where

import Halogen as H
import Halogen.HTML hiding (map)
import Halogen.HTML.Properties as HP
import Helper (styleClass)
import Import hiding (div)

authGoogleUrl :: String
authGoogleUrl = "/auth/page/googleemail2/forward"

data Slot = Slot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

data Input a = Noop a

type State =
  { loading :: Boolean
  , error :: Maybe String
  }

ui :: forall m. H.Component HTML Input Unit Void m
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
    where

      initialState = { loading: false, error: Nothing }

      render :: State -> H.ComponentHTML Input
      render st =
        div [ styleClass "container page" ]
          [ div [ styleClass "row" ]
            [ div [ styleClass "col-lg-12" ]
              [ text (if st.loading then "loading..." else "")
              , a [ HP.href authGoogleUrl ] [ text "Sign in with Google"]
              ]
            ]
          ]

      eval :: Input ~> H.ComponentDSL State Input Void m
      eval (Noop next) = pure next
