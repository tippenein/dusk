module Component.Admin.Curator where

import App.Data.Curator

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM.Event.Event (preventDefault)
import DOM.Event.Types as DOM
import Data.Argonaut (encodeJson)
import FormValidation (FormValue, FormValueG, Validator, formErrorHTML, formValueHTML, initFormValue, updateFormValue, validateA, validator)
import Halogen as H
import Halogen.HTML hiding (map)
import Halogen.HTML.Elements (input) as HE
import Halogen.HTML.Events (onSubmit, input)
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as HP
import Helper (apiUrl, styleClass)
import Import hiding (div)
import Network.HTTP.Affjax as AX
import Text.Email.Validate (EmailAddress(..), emailAddress)
import Top.Monad

data Slot = Slot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


emailValidator :: Validator String EmailAddress
emailValidator = validator go
  where
    go :: String -> Either String EmailAddress
    go email = case emailAddress email of
      Nothing -> Left "invalid email"
      Just e -> Right e

-- | The state of the application
newtype State = State { email :: FormValue String EmailAddress
                      }
initialState :: State
initialState = State { email: initFormValue emailValidator ""
                     }

-- | Inputs to the state machine
data Input a
  = FormSubmit a
  | UpdateEmail String a
  | PreventDefault DOM.Event a
  | NoAction a

simpleTextInput state field name action =
        div [ styleClass "row" ]
                [ div [ styleClass "col-md-4" ]
                        [ label [ HP.for field ] [ text name ] ]
                , div [ styleClass "col-md-4" ]
                        [ HE.input [ HP.name field
                                , formValueHTML state.email
                                , E.onValueChange (E.input action)
                                ]
                        ]
                , div [ styleClass "col-md-4" ]
                        [ p [ styleClass "form-error" ]
                              [ formErrorHTML state.email ]
                        ]
                ]
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
        State state <- H.get
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
        H.modify (\(State state) -> State state { email = updateFormValue state.email email })
        pure next

render :: State -> H.ComponentHTML Input
render (State state) = form [ onSubmit (input PreventDefault) ]
    [ simpleTextInput state "email" "Email" UpdateEmail
    , div [ styleClass "row" ]
            [ div [ styleClass "col-md-12" ]
                    [ button [ E.onClick (E.input_ FormSubmit)]
                                [ text "Submit" ]
                    ]
            ]
    ]

encodeBody = undefined
