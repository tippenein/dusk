module Component.Event where

import Prelude hiding (div)

import Control.Monad.Aff (Aff)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Halogen as H
import Halogen.HTML hiding (map)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Data.Formatter.DateTime as FD
import Data.DateTime as DateTime

import App.Data.Event (Event(..), Events(..), decodeEvents)

formatDateTime ∷ DateTime.DateTime -> Maybe String
formatDateTime x = hush $ FD.formatDateTime "AA, MMM D" x

type State =
  { loading :: Boolean
  , events :: Array Event
  , error :: Maybe String
  }

data Query a
  = GetEventList a
  | SelectEvent Int a

ui :: forall eff. H.Component HTML Query Unit Void (Aff (ajax :: AX.AJAX | eff))
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { loading: false, events: [], error: Nothing }

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AX.AJAX | eff))
  eval = case _ of
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

apiUrl :: String
apiUrl = "http://localhost:3000"

render :: State -> H.ComponentHTML Query
render st =
  div [ HP.class_ $ ClassName "home-page" ]
    [ viewBanner
    , viewErrors st.error
    , div [ styleClass "container page" ]
      [ div [ styleClass "row" ]
        [ div [ styleClass "col-lg-12" ]
          [ text (if st.loading then "loading..." else "") ]
          , viewEvents st.events
          ]
      ]
    ]

viewErrors (Just error) = 
  div [ styleClass "container" ]
    [ div [ styleClass "row" ]
      [ div [styleClass "alert alert-warning"] [ text error]]]
viewErrors Nothing = text ""

viewBanner =
    div [ styleClass "banner masthead" ]
        [ div [ styleClass "container" ]
          [ div [ styleClass "row" ]
            [ h1 [ styleClass "header logo-font" ] [ text "Dusk" ]
            , h2_ [ text "Curated Nightlife" ]
            ]
          ]
        ]


viewEvents events =
  div [ styleClass "event-list" ] (map viewEvent events)

viewEvent :: forall t. Event -> HTML t (Query Unit)
viewEvent (Event event) = eventRow timeContent b c
  where
  timeContent = [ p_ [ text (fromMaybe "TBD" (formatDateTime =<< event.start_datetime))] ]
  b = [ a [ styleClass "event-image event-default"
          , HP.href "javascript:void(0)"
          , HE.onClick (HE.input_ (SelectEvent event.id)) ]
        [ img [ HP.src event.asset_id, HP.height 250, HP.width 250 ]  ]
      ]
  c = [ h2_ [ text event.name ], p_ [ text event.description] ]

eventRow a b c =
  div [ styleClass "row" ]
    [ div [ styleClass "col-sm-3" ]
      [ h3 [ styleClass "event-time" ] a ]
    , div [ styleClass "col-sm-3" ]
      [ h3_ b ]
    , div [ styleClass "col-sm-6" ]
      [ h3 [ styleClass "event-info" ] c ]
    ]
styleClass = HP.class_ <<< ClassName
