module App.Route where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Control.Alt ((<|>))
import Data.Either (either)
-- import Routing (match)
import Halogen as Halogen
import Halogen.HTML hiding (map)
import Halogen.HTML.Properties as HP
import Control.Monad.Eff.Console as Console

type Event = { name :: String }
type State = { events :: Array Event, selected_event :: Maybe Event }

data Query a = ChangeRoute String a

data Location
  = Home
  | NotFound

pathFromRoute :: Location -> String
pathFromRoute = case _ of
  Home      -> "/"
  NotFound  -> "/404"


matchRoute :: String -> Location
matchRoute _ = Home
-- matchRoute :: String -> Location
-- matchRoute = either (\_ -> NotFound) id <<< match router

-- router :: Match Location
-- router =
--   Home      <$  (lit "" *> lit "" *> end)
--   <|>
--   Logout    <$  (lit "" *> lit "logout")

component :: forall m. Halogen.Component HTML Query Unit Void m
component =
  Halogen.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { events: [], selected_event: Nothing }

  render :: State -> Halogen.ComponentHTML Query
  render state =
    div_
      [ p_ [ text "Change the URL hash or choose an anchor link..." ]
      , ul_
          [ li_ [ a [ HP.href "#events" ] [ text "Events" ] ]
          , li_ [ a [ HP.href "#" ] [ text "Home" ] ]
          ]
      , ol_ $ map (\e -> eventView e) state.events
      ]

  eval :: Query ~> Halogen.ComponentDSL State Query Void m
  eval = case _ of
    ChangeRoute msg next -> do
      Halogen.modify \st -> { events: st.events, selected_event: Array.head st.events}
      pure next


eventView e = li_ [ text e.name ]
