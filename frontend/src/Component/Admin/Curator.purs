module Component.Admin.Curator where

import App.CodeGen (CuratorForm(..), _cf_invitee)
import Data.Lens (Lens', lens, (%~), (^.))
import Control.Monad.Aff.Console (log)
import DOM.Event.Event (preventDefault)
import DOM.Event.Types as DOM
import Data.Argonaut.Generic.Aeson (encodeJson, decodeJson)
import Handler.Crud
import Halogen as H
import Halogen.HTML hiding (map)
import Helper
import Import hiding (div)
import Network.HTTP.Affjax as AX
import Text.Email.Validate (emailAddress)
import Top.Monad (Top)
import WForm as Form

data Slot = Slot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


-- | The state of the application
type State =
  { form :: CuratorForm
  , sending :: Boolean
  }

_form :: Lens' State CuratorForm
_form = lens _.form _ { form = _}

initialForm :: CuratorForm
initialForm = CuratorForm { cf_invitee: "" }

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
          let email = state ^. (_form <<< _cf_invitee)
          case emailAddress email of
            Nothing -> H.liftAff $ log $ "invalid email"
            Just e -> do
              H.modify (_ { sending = true })
              response <- H.liftAff $ AX.post (apiUrl <> "/admin/curators") (encodeJson state.form)
              H.modify (_ { sending = false })
              let Tuple typ msg = handleCreateResponse response
              H.liftEff $ flashMessage typ msg
        handleNewCurator (Form.Edit f) = do
            H.modify (_form %~ f)

handleCreateResponse res =
  case decodeJson res.response of
    Left e -> Tuple Failure e
    Right cr -> case cr of
      CreateSuccess _ -> Tuple Success "Sent invite"
      FailedUniquenessConstraint -> Tuple Warning "You've already sent this person an invite"
      CreateFailure t -> Tuple Failure t

render :: State -> H.ComponentHTML Input
render state =
  div [ styleClass "admin--form centered"] [
      h3 [] [ text "Invite" ]
    , p_ [ text "Send an email inviting your coolest curator to the platform" ]
    , div_ $ Form.renderForm state.form NewCurator do
        Form.textField "email" "Email" (_cf_invitee) (Form.nonBlank <=< Form.emailValidator)
  ]
