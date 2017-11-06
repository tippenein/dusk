module Component.Auth where

import Halogen as H
import Halogen.HTML (HTML, a, div, text)
import Halogen.HTML.Properties as HP
import Helper (styleClass)
import Import (Maybe(..))
import Prelude hiding (div)
import Top.Monad (Top)
import Routes as Routes

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

ui :: H.Component HTML Input Unit Routes.ChildAction Top
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
            [ div [ styleClass "col-lg-12 center" ]
              [ text (if st.loading then "loading..." else "")
              , a [ styleClass "loginBtn loginBtn--google", HP.href authGoogleUrl ] [ text "Sign in with Google"]
              ]
            ]
          ]

      eval :: Input ~> H.ComponentDSL State Input Routes.ChildAction Top
      eval (Noop next) = pure next
