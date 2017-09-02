module Component.Admin.Curator where

import App.Data.Curator

import Control.Monad.Aff.Console (log)
import DOM.Event.Event (preventDefault)
import DOM.Event.Types as DOM
import Data.Argonaut (encodeJson)
import FormValidation (FormValue, FormValueG, Validator, formErrorHTML, formValueHTML, initFormValue, updateFormValue, validateA, validator)
import Halogen as H
import Halogen.HTML hiding (map)
import Halogen.HTML.Elements as Elements
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as HP
import Helper (apiUrl, styleClass)
import Helper.Form as Form
import Import hiding (div)
import Network.HTTP.Affjax as AX
import Text.Email.Validate (EmailAddress(..), emailAddress)
import Top.Monad (Top)

data Slot = Slot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


-- | The state of the application
type State =
  { email :: FormValue String EmailAddress
  , sending :: Boolean
  }
initialState :: State
initialState = { sending: false
               , email: initFormValue Form.emailValidator "" }

-- | Inputs to the state machine
data Input a
  = FormSubmit a
  | UpdateEmail String a
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
    eval (FormSubmit next) = do
        state <- H.get
        email <- runExceptT $ validateA state.email
        case email of
          Left err ->
            H.liftAff $
                log $ "invalid email" <> err
          Right e -> do
            let invite = CuratorInvite { invitee: e, inviter: 1}
            response <- H.liftAff $ AX.post (apiUrl <> "/admin/curators") (encodeJson invite)
            H.liftAff $ log $ "invite was sent: " <> response.response
        pure next
    eval (UpdateEmail email next) = do
        H.modify (\st -> st { email = updateFormValue st.email email })
        pure next

render :: State -> H.ComponentHTML Input
render state = form [ E.onSubmit (E.input PreventDefault) ]
  [ Form.simpleTextInput state.email "email" "Email" UpdateEmail
  , Form.formSubmit "Submit" FormSubmit state.sending
  ]
