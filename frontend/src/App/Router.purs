module App.Router where

import Routing (matchesAff)
import Routing.Match (Match)
import Routing.Match.Class (num, int, lit)
import Halogen as H
import Network.HTTP.Affjax as AX
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import Control.Monad.State.Class (modify)
import Halogen.Component.ChildPath (ChildPath, cpR, cpL)
import Halogen.HTML hiding (map)
import Data.String as Str


import Import hiding (div)
import Component.Event as Event
import Component.Profile as Profile

data Location
  = HomeR
  | Profile
  | CuratorsR
  | EventsR
  | EventR Int

-- derive instance genericLocation :: Generic Location _

instance showLocation :: Show Location where
  show HomeR = "home"
  show Profile = "profile"
  show CuratorsR = "curators"
  show EventsR = "events"
  show (EventR _) = "events"

instance eqLocation :: Eq Location where
  eq a b = show a == show b

data Input a
  = Goto Location a

oneSlash :: Match Unit
oneSlash = lit "/"

homeSlash :: Match Unit
homeSlash = lit ""

data CRUD
  = Index
  | Show Number

routing :: Match Location
routing =
  events <|>
  event  <|>
  home
  where
    home = HomeR <$ lit ""
    events = EventsR <$ lit "events"
    event = EventR <$> (homeSlash *> lit "events" *> int)


type State =
  { currentPage :: Location
  , error :: Maybe String
  }

type ChildQuery = Coproduct Profile.Input Event.Input
type ChildSlot = Either Profile.Slot Event.Slot

pathToProfile :: ChildPath Profile.Input ChildQuery Profile.Slot ChildSlot
pathToProfile = cpL

pathToEvents :: ChildPath Event.Input ChildQuery Event.Slot ChildSlot
pathToEvents = cpR

type QueryP
  = Coproduct Input ChildQuery

ui :: forall eff. H.Component HH.HTML Input Unit Void (Aff (ajax :: AX.AJAX | eff ))
ui = H.lifecycleParentComponent
  { initialState: const init
  , initializer: Nothing
  , finalizer: Nothing
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render st =
      mainBody st (viewPage st.currentPage)

    init :: State
    init = { currentPage: HomeR, error: Nothing }

    -- viewPage :: String -> H.ParentHTML Input ChildQuery ChildSlot m
    viewPage Profile =
      HH.slot' pathToProfile Profile.Slot Profile.ui unit absurd
    viewPage EventsR = do
      HH.slot' pathToEvents Event.Slot Event.ui unit absurd
    viewPage _ =
      HH.div_ [ HH.text "Not Found"]

    eval :: Input ~> H.ParentDSL State Input ChildQuery ChildSlot Void  (Aff (ajax :: AX.AJAX | eff ))
    eval (Goto Profile next) = do
      modify (_ { currentPage = Profile })
      pure next
    eval (Goto CuratorsR next) = do
      modify (_ { currentPage = CuratorsR })
      pure next
    eval (Goto EventsR next) = do
      modify (_ { currentPage = EventsR })
      pure next
    eval (Goto (EventR i) next) = do
      modify (_ { currentPage = EventR i })
      pure next
    eval (Goto HomeR next) = do
      modify (_ { currentPage = HomeR })
      pure next

routeSignal :: forall eff. H.HalogenIO Input Void (Aff (HA.HalogenEffects eff))
            -> Aff (HA.HalogenEffects eff) Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  redirects driver old new

redirects :: forall eff. H.HalogenIO Input Void (Aff (HA.HalogenEffects eff))
          -> Maybe Location
          -> Location
          -> Aff (HA.HalogenEffects eff) Unit
redirects driver _old =
  driver.query <<< H.action <<< Goto



viewErrors (Just error) =
  div [ styleClass "container" ]
    [ div [ styleClass "row" ]
      [ div [styleClass "alert alert-warning"] [ text error]]]
viewErrors Nothing = text ""

styleClass = HP.class_ <<< ClassName

viewBanner =
    div [ styleClass "banner masthead" ]
        [ div [ styleClass "container" ]
          [ div [ styleClass "row" ]
            [ h1 [ styleClass "header logo-font" ] [ text "Dusk" ]
            , h2_ [ text "Curated Nightlife" ]
            ]
          ]
        ]


mainBody st sub =
  div [ HP.class_ $ ClassName "home-page" ]
    [ navbar st
    , viewBanner
    , viewErrors st.error
    , sub ]

navbar st =
  nav [ styleClass "navbar navbar-default navbar-static-top" ]
    [ div [ styleClass "container" ]
      [ div [ styleClass "navbar-header" ]
        [ button [ styleClass "navbar-toggle collapsed"]
          [ span [ styleClass "icon-bar"] []
          , span [ styleClass "icon-bar"] []
          , span [ styleClass "icon-bar"] [] ]
        ]
      , div [ HP.id_ "navbar", styleClass "collapse navbar-collapse" ]
        [ ul [ styleClass "nav navbar-nav"] 
          -- navbarItems st [(EventsR, "Events"), (CuratorsR, "Curators")]
          [ li [ checkActive st EventsR ] [ a [ HP.href "#events"] [text "Events"] ]
          , li [ checkActive st CuratorsR ][ a [ HP.href "#curators"] [text "Curators"] ]
          ]
        ]
      ]
    ]

checkActive st r = if st.currentPage == r then styleClass "active" else styleClass ""

navbarItems st routes = map f routes
  where f = (\(Tuple route routeName) ->
              [li
               [ checkActive st route ]
               [ a [ HP.href (Str.toLower routeName)] [text routeName] ]])
