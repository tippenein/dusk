module Component.Admin.Event where


import Control.Monad.Aff.Console (log)
import DOM.Event.Event (preventDefault)
import DOM.Event.Types as DOM
import Data.Argonaut.Generic.Aeson (encodeJson, decodeJson)
import Data.Lens (Lens', lens, (%~))
import FileUpload (fileUpload)
import Halogen as H
import Halogen.HTML hiding (map)
import Helper (Message(..), apiUrl, flashMessage, styleClass)
import Helper.Form (flatpicker)
import Import hiding (div)
import Message as Msg
import Network.HTTP.Affjax as AX
import Top.Monad (Top)
import Req (handleCreateResponse, handleUpdateResponse)
import App.Crud
import App.Form
import WForm as Form


data Slot = Slot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

_form :: Lens' State EventForm
_form = lens _.form _ { form = _ }

initialEventForm :: EventForm
initialEventForm = EventForm
  { ef_name: ""
  , ef_description: Nothing
  , ef_startDatetime: Nothing
  , ef_endDatetime: Nothing
  , ef_logo: Nothing
  }

type State =
  { loading :: Boolean
  , form :: EventForm
  }

data Input a
  = Noop a
  | PreventDefault DOM.Event a
  | NewEvent (Form.FormInput EventForm) a
  | SelectEvent Int a
  | PostRender a

ui :: H.Component HTML Input Unit Void Top
ui =
  H.lifecycleComponent
    { initialState: const initialState
    , initializer: Just (H.action PostRender)
    , finalizer: Nothing
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { loading: false, form: initialEventForm }

  eval :: Input ~> H.ComponentDSL State Input Void Top
  eval (Noop next) = pure next
  eval (PreventDefault e next) = H.liftEff (preventDefault e) $> next
  eval (NewEvent ev next) = handleNewEvent ev $> next
    where
      handleNewEvent (Form.Edit f) =
        H.modify (_form %~ f)
      handleNewEvent Form.Submit = do
        st <- H.get
        H.modify (_ { loading = true })
        response <- H.liftAff $ AX.post (apiUrl <> "/admin/events") (encodeJson st.form)
        H.modify (_ { loading = false })
        let Tuple typ msg = handleCreateResponse "admin.event" response
        H.liftEff $ flashMessage typ msg
        case typ of
          Success -> H.liftEff $ fileUpload "logo_asset" (mkLogoUrl msg)
          _ ->  H.liftAff $ log "did not upload asset"

  eval (PostRender next) = do
    H.liftEff $ flatpicker "#start_datetime"
    H.liftEff $ flatpicker "#end_datetime"
    pure next
  eval (SelectEvent _ next) = pure next

mkLogoUrl :: String -> String
mkLogoUrl i =  apiUrl <> "/admin/events/" <> i <> "/logo"

render :: State -> H.ComponentHTML Input
render st = do
  div [ styleClass "admin--form centered"] [
      h3 [] [ text Msg.eventFormHeader ]
    , p_ [ text Msg.eventFormHelp ]
    , div_ $
      Form.renderForm st.form NewEvent do
        void $ Form.textField "name" "Name" (_ef_name) Form.nonBlank
        void $ Form.textFieldOpt "description" "Description" (_ef_description) noopValidation
        void $ Form.textFieldOpt "start_datetime" "Start" (_ef_startDatetime) Form.nonBlank
        void $ Form.textFieldOpt "end_datetime" "End" (_ef_endDatetime) noopValidation
        Form.fileFieldOpt "logo_asset" "Logo" (_ef_logo) noopValidation
  ]

noopValidation = Right
