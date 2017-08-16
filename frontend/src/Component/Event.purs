module Component.Event where

import Prelude hiding (div)

import Control.Monad.Aff (Aff)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML hiding (map)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX

import App.Data.Event (Event(..), Events(..), decodeEvents)


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
  -- loaded <- ?liftAff AffUtil.awaitLoad
  div [ HP.class_ $ ClassName "home-page" ]
    [ viewBanner
    , div [ styleClass "container page" ]
      [ div [ styleClass "row" ]
        [ div [ styleClass "col-md-12" ]
          [ text (if st.loading then "loading..." else "") ]
          , viewEvents st.events
          ]
      ]
    ]

viewBanner =
    div [ styleClass "banner" ]
        [ div [ styleClass "container" ]
            [ h1 [ styleClass "logo-font" ] [ text "dusk" ]
            , p_ [ text "Curated events" ]
            ]
        ]


viewEvents events =
  div [ styleClass "event-list" ] (map viewEvent events)
  where
  viewEvent (Event event) =
    div_
      [ a [ styleClass "event-link event-default"
          , HP.href "javascript:void(0)"
          , HE.onClick (HE.input_ (SelectEvent event.id)) ]
        [ text event.name ]
      ]

styleClass = HP.class_ <<< ClassName

-- view model =
--     div_ [ class "home-page" ]
--         [ viewBanner
--         , div [ class "container page" ]
--             [ div [ class "row" ]
--                 [ div [ class "col-md-12" ]
--                     [ div [ class "sidebar" ]
--                         [ p [] [ text "Popular Events" ]
--                         , p [] [ text "events here" ]
--                         ]
--                     ]
--                 ]
--             ]
--         ]
