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
import Helper (apiUrl, placeholder, styleClass)
import Helper.Format (unformatDateTime)
import Import hiding (div)
import Network.HTTP.Affjax as AX
import Top.Monad (Top)


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
  timeContent = [ p_ [ text (fromMaybe "TBA" (formatDateTime =<< (unformatDateTime =<< event.start_datetime)))] ]
  b = [ a [ styleClass "event-image event-default"
          , HP.href "javascript:void(0)"
          , HE.onClick (HE.input_ (SelectEvent event.id)) ]
        [ maybeImageSrc event.asset_id 250 250 ]
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

maybeImageSrc (Just s) h w = img [ HP.src s, HP.height h, HP.width w ]
maybeImageSrc Nothing h w = placeholder h w

truncated (Just s) =
  div_ [ text s', ellipsisLink "#events" ]
  where s' = if Str.length s > 140
             then (Str.take 140 s)
             else s
truncated Nothing = div_ [ text "We don't know anything about this event yet" ]

ellipsisLink e = a [ HP.href e ] [ text "..."]
