module Component.Event where

import App.Data.Event (Event(..), Events(..), decodeEvents)
import App.Data.Profile (Profile(..), decodeProfile)
import Routes as Routes
import Data.DateTime as DateTime
import Data.Formatter.DateTime as FD
import Data.String as Str
import Halogen as H
import Halogen.HTML (HTML, a, div, div_, h2_, h3, h3_, img, p_, text)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Helper (col, apiUrl, placeholder, styleClass)
import Helper.Format (unformatDateTime)
import Import hiding (div)
import Network.HTTP.Affjax as AX
import Top.Monad (Top)


data Slot = Slot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

formatDateTime :: DateTime.DateTime -> Maybe String
formatDateTime x = hush $ FD.formatDateTime "ddd, MMM D" x

type State =
  { loading :: Boolean
  , events :: Array Event
  , focus :: Maybe Event
  , currentUser :: Maybe Profile
  , error :: Maybe String
  }

data Input a
  = Noop a
  | GetEventList a
  | Edit Event a
  | SelectEvent Event a
  | Unfocus a

ui :: H.Component HTML Input Unit Routes.ChildAction Top
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
  initialState =
    { loading: false, events: [], error: Nothing, focus: Nothing, currentUser: Nothing }

  eval :: Input ~> H.ComponentDSL State Input Routes.ChildAction Top
  eval = case _ of
    Noop next -> pure next
    Edit e next -> pure next -- pure $ Routes.Goto Routes.AdminR next
    GetEventList next -> do
      H.modify (_ { loading = true })
      response <- H.liftAff $ AX.get (apiUrl <> "/profile")
      user <- pure $ decodeProfile response.response
      H.modify (_ { currentUser = hush user })
      response <- H.liftAff $ AX.get (apiUrl <> "/events")
      es <- pure $ decodeEvents response.response
      case es of
        Left e ->
          H.modify (_ { error = Just e, loading = false, events = []})
        Right (Events {events: res}) ->
          H.modify (_ { error = Nothing, loading = false, events = res})
      pure next

    Unfocus next -> do
      H.modify (_ { focus = Nothing })
      pure next
    SelectEvent e next -> do
      H.modify (_ { focus = Just e })
      pure next

render :: State -> H.ComponentHTML Input
render st =
  div [ styleClass "container page" ]
    [ div [ styleClass "row" ]
      [ div [ styleClass "col-lg-12" ]
        [ text (if st.loading then "loading..." else "") ]
        , viewEvents st.currentUser st.focus st.events
        ]
    ]
  where
    viewEvents user_id (Just event) _ =
      div [ styleClass "event-detail" ] [ eventDetail user_id event ]
    viewEvents _ Nothing events =
      div [ styleClass "event-list" ] (map eventPreview events)

eventNav currentUser event =
  div [ styleClass "row" ]
      [ back
      , div [ styleClass "text-right"] [ edit event ]
      ]
  where
    muser_id (Just (Profile p)) = p.user_id
    muser_id Nothing = 0
    edit e = if muser_id currentUser == event.owner_id then a [HE.onClick $ HE.input_ (Edit (Event e))] [ text "edit" ] else p_ [ text ""]
    back = a [ HE.onClick (HE.input_ Unfocus)] [ text "back"]

eventDetail :: forall t. Maybe Profile -> Event -> HTML t (Input Unit)
eventDetail currentUser (Event event) =
  div [] [
      eventNav currentUser event
    , div [ styleClass "row" ]
      [ col 3
        [ h3 [ styleClass "event-time" ] [ timeContent ]
        ]
      , col 9
        [ header
        , im
        ]
      ]
    ]
  where
    timeContent = p_ [ text (fromMaybe "TBA" (formatDateTime =<< (unformatDateTime =<< event.start_datetime)))]
    im = a [ styleClass "event-image event-default" ]
          [ maybeImageSrc event.asset_id 250 250 ]
    header = col 3 [h2_ [ text event.name ], p_ [ truncated event.description ]]

eventPreview :: forall t. Event -> HTML t (Input Unit)
eventPreview (Event event) = eventRow timeContent b c
  where
  timeContent = [ p_ [ text (fromMaybe "TBA" (formatDateTime =<< (unformatDateTime =<< event.start_datetime)))] ]
  b = [ a [ styleClass "event-image event-default"
          -- , HP.href "javascript:void(0)"
          , HE.onClick (HE.input_ (SelectEvent (Event event))) ]
        [ maybeImageSrc event.asset_id 250 250 ]
      ]
  c = [ h2_ [ text event.name ], p_ [ truncated event.description ] ]

  eventRow leftCol middleCol rightCol =
    div [ styleClass "row" ]
      [ col 3
        [ h3 [ styleClass "event-time" ] leftCol ]
      , col 3
        [ h3_ middleCol ]
      , col 6
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
