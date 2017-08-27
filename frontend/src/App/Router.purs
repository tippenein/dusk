module App.Router where

import Component.Admin.Main as Admin
import Component.Auth as Auth
import Component.Curator as Curator
import Component.Event as Event
import Component.NotFound as NotFound
import Control.Monad.Aff (Aff)
import Control.Monad.State.Class (modify)
import Data.Either.Nested (Either4)
import Data.Functor.Coproduct.Nested (Coproduct4)
import Halogen as H
import Halogen.Component.ChildPath (ChildPath, cp1, cp2, cp3, cp4)
import Halogen.HTML as HH
import Halogen.HTML hiding (map)
import Halogen.HTML.Properties as HP
import Helper (styleClass, styleClassIf)
import Import hiding (div)
import Routing (matchesAff)
import Routing.Match (Match)
import Routing.Match.Class (int, lit, fail)
-- import Component.Profile as Profile

data Location
  = HomeR
  | Profile
  | CuratorsR
  | EventsR
  | LoginR
  | AdminR
  | EventR Int
  | NotFoundR String



instance showLocation :: Show Location where
  show HomeR = "home"
  show Profile = "profile"
  show CuratorsR = "curators"
  show LoginR = "login"
  show EventsR = "events"
  show AdminR = "admin"
  show (EventR _) = "events"
  show (NotFoundR s) = s

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
  events      <|>
  admins      <|>
  login       <|>
  curators    <|>
  event       <|>
  home        <|>
  notFound
  where
    home = HomeR <$ lit ""
    login = LoginR <$ lit "login"
    events = EventsR <$ lit "events"
    admins = AdminR <$ lit "admin"
    adminPath = lit "admin" *> homeSlash
    event = EventR <$> (homeSlash *> lit "events" *> int)
    curators = CuratorsR <$ lit "curators"
    notFound = NotFoundR <$> fail "Not Found"


type State =
  { currentPage :: Location
  , error :: Maybe String
  }

type ChildQuery = Coproduct4 Curator.Input Event.Input Auth.Input Admin.Input
type ChildSlot = Either4 Curator.Slot Event.Slot Auth.Slot Admin.Slot

pathToCurators :: ChildPath Curator.Input ChildQuery Curator.Slot ChildSlot
pathToCurators = cp1

pathToEvents :: ChildPath Event.Input ChildQuery Event.Slot ChildSlot
pathToEvents = cp2

pathToAuth :: ChildPath Auth.Input ChildQuery Auth.Slot ChildSlot
pathToAuth = cp3

pathToAdmin :: ChildPath Admin.Input ChildQuery Admin.Slot ChildSlot
pathToAdmin = cp4

ui :: H.Component HH.HTML Input Unit Void Top
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
    init = { currentPage: EventsR, error: Nothing }

    viewPage :: Location -> H.ParentHTML Input ChildQuery ChildSlot Top
    viewPage AdminR = do
      HH.slot' pathToAdmin Admin.Slot Admin.ui unit absurd
    viewPage LoginR = do
      HH.slot' pathToAuth Auth.Slot Auth.ui unit absurd
    viewPage EventsR = do
      HH.slot' pathToEvents Event.Slot Event.ui unit absurd
    viewPage CuratorsR = do
      HH.slot' pathToCurators Curator.Slot Curator.ui unit absurd
    viewPage HomeR =
      HH.slot' pathToEvents Event.Slot Event.ui unit absurd
    viewPage s = NotFound.view (show s)

    eval :: Input ~> H.ParentDSL State Input ChildQuery ChildSlot Void Top
    eval (Goto Profile next) = do
      modify (_ { currentPage = Profile })
      pure next
    eval (Goto CuratorsR next) = do
      modify (_ { currentPage = CuratorsR })
      pure next
    eval (Goto EventsR next) = do
      modify (_ { currentPage = EventsR })
      pure next
    eval (Goto AdminR next) = do
      modify (_ { currentPage = AdminR })
      pure next
    eval (Goto LoginR next) = do
      modify (_ { currentPage = LoginR })
      pure next
    eval (Goto (EventR i) next) = do
      modify (_ { currentPage = EventR i })
      pure next
    eval (Goto HomeR next) = do
      modify (_ { currentPage = HomeR })
      pure next
    eval (Goto (NotFoundR s) next) = do
      modify (_ { currentPage = (NotFoundR s) })
      pure next

routeSignal :: H.HalogenIO Input Void (Aff TopEffects)
            -> Aff TopEffects Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  redirects driver old new
  where
  redirects driver _old =
    driver.query <<< H.action <<< Goto



viewErrors (Just error) =
  div [ styleClass "container" ]
    [ div [ styleClass "row" ]
      [ div [styleClass "alert alert-warning"] [ text error]]]
viewErrors Nothing = text ""

viewBanner =
    div [ styleClass "banner masthead" ]
        [ div [ styleClass "container" ]
          [ div [ styleClass "row" ]
            [ h1 [ styleClass "header logo" ] [ text "DUSK" ]
            , h2_ [ text "Curated Nightlife" ]
            ]
          ]
        ]

mainBody st sub =
  div [ HP.class_ $ ClassName "home-page" ]
    [ navbar st
    , if st.currentPage == HomeR then viewBanner else p_ [text ""]
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
          [ li [ checkActiveLogo st HomeR ] [ a [ HP.href "/" ] [text "DUSK"] ]
          , li [ checkActive st EventsR ] [ a [ HP.href "#events"] [text "Events"] ]
          , li [ checkActive st CuratorsR ][ a [ HP.href "#curators"] [text "Curators"] ]
          ]
        ]
      ]
    ]
  where
    checkActiveLogo st r = styleClassIf (isActive st r) "logo"
    checkActive st r = if isActive st r then styleClass "active" else styleClass ""
    isActive st r = st.currentPage == r

--   navbarItems st routes = map f routes
--     where f = (\(Tuple route routeName) ->
--                 [li
--                  [ checkActive st route ]
--                  [ a [ HP.href (Str.toLower routeName)] [text routeName] ]])
