module Component.Admin.Main where


import Component.Admin.Curator as Curator
import Component.Admin.Event as Event
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Halogen as H
import Halogen.Component.ChildPath (ChildPath, cp1, cp2)
import Halogen.HTML hiding (map)
import Halogen.HTML.Events (input_, onClick)
import Import hiding (div)
import Helper
import Routes as Routes

data Slot = Slot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

data Tab
  = Invite
  | Announce

derive instance eqTab :: Eq Tab

type State =
  { loading :: Boolean
  , error :: Maybe String
  , activeTab :: Tab
  }

data Input a
  = Noop a
  | ShowInvite a
  | ShowAnnounce a

type ChildQuery = Coproduct2 Curator.Input Event.Input
type ChildSlot = Either2 Curator.Slot Event.Slot

pathToCurators :: ChildPath Curator.Input ChildQuery Curator.Slot ChildSlot
pathToCurators = cp1

pathToEvents :: ChildPath Event.Input ChildQuery Event.Slot ChildSlot
pathToEvents = cp2

ui :: H.Component HTML Input Unit Routes.ChildAction Top
ui =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState = { loading: false, error: Nothing, activeTab: Announce }

  eval :: Input ~> H.ParentDSL State Input ChildQuery ChildSlot Routes.ChildAction Top
  eval = case _ of
    Noop next -> pure next
    ShowInvite next -> do
      H.modify (_ { activeTab = Invite })
      pure next
    ShowAnnounce next -> do
      H.modify (_{ activeTab = Announce })
      pure next

  render st =
    div [ styleClass "container admin--page" ]
      [ ul [ styleClass "nav nav-tabs" ]
        [ li [ styleClassIf (st.activeTab == Announce) "active" ]
          [ a [ onClick (input_ ShowAnnounce) ] [ text "Announce" ] ]
        , li [ styleClassIf (st.activeTab == Invite) "active" ]
          [ a [ onClick (input_ ShowInvite) ] [ text "Invite" ] ]
        ]
      , div [ styleClass "col-md-4 col-md-offset-4 centered" ]
        [ div [ styleClassIf (st.activeTab /= Announce) "hidden" ]
          [ slot' pathToEvents Event.Slot Event.ui unit (\_ -> Nothing) ]
        , div [ styleClassIf (st.activeTab /= Invite) "hidden" ]
          [ slot' pathToCurators Curator.Slot Curator.ui unit (\_ -> Nothing)]
        ]
      ]
