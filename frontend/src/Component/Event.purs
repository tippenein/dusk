module Component.Event where

import App.Data.Event (Event(..), Events(..), decodeEvents)
import Control.Monad.Aff (Aff)
import Data.DateTime as DateTime
import Data.Formatter.DateTime as FD
import Data.String as Str
import Halogen as H
import Halogen.HTML (HTML, a, div, div_, h2_, h3, h3_, img, p_, text)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX

import Top.Monad (Top)
import Helper (apiUrl, styleClass)
import Import hiding (div)


data Slot = Slot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

formatDateTime ∷ DateTime.DateTime -> Maybe String
formatDateTime x = hush $ FD.formatDateTime "AA, MMM D" x

type State =
  { loading :: Boolean
  , events :: Array Event
  , error :: Maybe String
  }

data Input a
  = Noop a
  | GetEventList a
  | SelectEvent Int a

ui :: H.Component HTML Input Unit Void Top
ui =
  H.lifecycleComponent
    { initialState: const initialState
    , initializer: Just (H.action GetEventList)
    , finalizer: Nothing
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { loading: false, events: [], error: Nothing }

  eval :: Input ~> H.ComponentDSL State Input Void Top
  eval = case _ of
    Noop next -> pure next
    GetEventList next -> do
      events <- H.gets _.events
      H.modify (_ { loading = true })
      response <- H.liftAff $ AX.get (apiUrl <> "/events")
      es <- pure $ decodeEvents response.response
      case es of
        Left e ->
          H.modify (_ { error = Just e, loading = false, events = events })
        Right (Events {events: res}) ->
          H.modify (_ { error = Nothing, loading = false, events = res})
      pure next

    SelectEvent _ next -> do
      pure next

render :: State -> H.ComponentHTML Input
render st =
  div [ styleClass "container page" ]
    [ div [ styleClass "row" ]
      [ div [ styleClass "col-lg-12" ]
        [ text (if st.loading then "loading..." else "") ]
        , viewEvents st.events
        ]
    ]
  where
    viewEvents events =
      div [ styleClass "event-list" ] (map viewEvent events)



viewEvent :: forall t. Event -> HTML t (Input Unit)
viewEvent (Event event) = eventRow timeContent b c
  where
  timeContent = [ p_ [ text (fromMaybe "TBA" (formatDateTime =<< event.start_datetime))] ]
  b = [ a [ styleClass "event-image event-default"
          , HP.href "javascript:void(0)"
          , HE.onClick (HE.input_ (SelectEvent event.id)) ]
        [ img [ HP.src event.asset_id, HP.height 250, HP.width 250 ]  ]
      ]
  c = [ h2_ [ text event.name ], p_ [ truncated event.description ] ]

  eventRow leftCol middleCol rightCol =
    div [ styleClass "row" ]
      [ div [ styleClass "col-sm-3" ]
        [ h3 [ styleClass "event-time" ] leftCol ]
      , div [ styleClass "col-sm-3" ]
        [ h3_ middleCol ]
      , div [ styleClass "col-sm-6" ]
        [ h3 [ styleClass "event-info" ] rightCol ]
      ]

truncated s =
  div_ [ text s', ellipsisLink "#" ]
  where s' = if Str.length s > 140
             then (Str.take 140 s)
             else s

ellipsisLink e = a [ HP.href e ] [ text "..."]
