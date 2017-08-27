module Component.Admin.Event where

import Helper

import App.Data.Event as Event
import Control.Monad.Aff (Aff)
import Data.DateTime as DateTime
import Data.Formatter.DateTime as FD
import Halogen as H
import Halogen.HTML hiding (map)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Import hiding (div)
import Network.HTTP.Affjax as AX
import Top.Monad (Top)


data Slot = Slot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

parseDateTime x = hush $ FD.unformatDateTime "AA, MMM D" x

type EventForm = { name :: String }

type State =
  { loading :: Boolean
  , form :: Maybe EventForm
  , error :: Maybe String
  }

data Input a
  = Noop a
  | Submit a
  | SelectEvent Int a

ui :: H.Component HTML Input Unit Void Top
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { loading: false, form: Nothing, error: Nothing }

  eval :: Input ~> H.ComponentDSL State Input Void Top
  eval = case _ of
    Noop next -> pure next
    Submit next -> do
      form <- H.gets _.form
      H.modify (_ { loading = true })
      -- response <- H.liftAff $ AX.post (apiUrl <> "/admin/events") (toPostData form)
      -- es <- pure $ Event.decodeCreateResponse response.response
      -- case es of
      --   Left e ->
      --     H.modify (_ { error = Just e, loading = false, form = Nothing})
      --   Right (Event.CreateResponse {event: res}) ->
      --     H.modify (_ { error = Nothing, loading = false, form = res})
      pure next

    SelectEvent _ next -> do
      pure next

-- toPostData :: EventForm -> _
-- toPostData e = undefined

render :: State -> H.ComponentHTML Input
render st =
  div [ styleClass "container page" ]
    [ div [ styleClass "row" ]
      [ div [ styleClass "col-lg-12" ]
        [ text (if st.loading then "posting..." else "") ]
        -- , viewEventForm
        ]
    ]


viewEventForm = undefined
