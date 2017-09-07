module Component.Admin.Curator where

import App.Data.Curator

import Control.Monad.Aff.Console (log)
import DOM.Event.Event (preventDefault)
import DOM.Event.Types as DOM
import Data.Argonaut (encodeJson)
import Data.Lens ((%~), Lens', lens)
import Halogen as H
import Halogen.HTML hiding (map)
import Halogen.HTML.Elements as Elements
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as HP
import Helper (apiUrl, styleClass)
import WForm as Form
import Import hiding (div)
import Network.HTTP.Affjax as AX
import Text.Email.Validate (EmailAddress(..), emailAddress)
import Top.Monad (Top)

data Slot = Slot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


-- | The state of the application
type State =
  { form :: CuratorForm
  , sending :: Boolean
  }

type CuratorForm = { email :: String }

_email :: Lens' CuratorForm String
_email = lens _.email _ { email = _ }

_CuratorForm :: Lens' State CuratorForm
_CuratorForm = lens _.form _ { form = _}

initialForm :: CuratorForm
initialForm = {email: ""}

initialState :: State
initialState = { sending: false
               , form: initialForm }

-- | Inputs to the state machine
data Input a
  = NewCurator (Form.FormInput CuratorForm) a
  | PreventDefault DOM.Event a
  | NoAction a


ui :: H.Component HTML Input Unit Void Top
ui =
  H.component
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  }
  where

    eval :: Input ~> H.ComponentDSL State Input Void Top
    eval (NoAction next) = pure next
    eval (PreventDefault e next) = H.liftEff (preventDefault e) $> next
    eval (NewCurator ev next) = handleNewCurator ev $> next
      where
        handleNewCurator (Form.Submit) = do
          state <- H.get
          H.liftAff $ log $ "blerble"
          -- case _email of
          --   Left err ->
          --     H.liftAff $
          --         log $ "invalid email" <> err
          --   Right e -> do
          --     let invite = CuratorInvite { invitee: e, inviter: 1}
          --     response <- H.liftAff $ AX.post (apiUrl <> "/admin/curators") (encodeJson invite)
          --     H.liftAff $ log $ "invite was sent: " <> response.response
        handleNewCurator (Form.Edit f) = do
            H.modify (_CuratorForm %~ f)

render :: State -> H.ComponentHTML Input
render state =
  div [ styleClass "admin--form centered"] [
      h3 [] [ text "Invite" ]
    , p_ [ text "Send an email inviting your coolest curator to the platform" ]
    , div_ $ Form.renderForm state.form NewCurator do
        Form.textField "email" "Email" (_email) (Form.nonBlank <=< Form.emailValidator)
      -- , Form.formSubmit "Submit" Form.Submit state.sending
  ]
