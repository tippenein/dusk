module Component.Admin.Event where


import Component.Admin.Event.Form
import Helper

import App.Data.Event (EventCreateResponse(..), decodeEventCreateResponse)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import DOM.Event.Event (preventDefault)
import DOM.Event.Types as DOM
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Argonaut.Core (fromString)
import Data.Argonaut.Encode (encodeJson)
import Data.DateTime (DateTime(..))
import Data.DateTime as DateTime
import Data.Formatter.DateTime as FD
import Data.Generic (gShow)
import Data.Lens (Lens', lens, (%~))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
import FileUpload (fileUpload)
import FormValidation (FormValue, formValueHTML, initFormValue, updateFormValue, validateA)
import Halogen as H
import Halogen.Datepicker.Component.DateTime as Time
import Halogen.Datepicker.Config (defaultConfig)
import Halogen.Datepicker.Format.DateTime as Format
import Halogen.HTML hiding (map)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as HP
import WForm as Form
import Helper.Format (formatDateTime, unformatDateTime)
import Import hiding (div)
import Message as Msg
import Network.HTTP.Affjax as AX
import Top.Monad (Top)
import Helper.Form (flatpicker)

data Slot = Slot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

_form :: Lens' State EventForm
_form = lens _.form _ { form = _ }

newtype EventRequest = EventRequest
  { name :: String
  , description :: Maybe String
  , startDatetime :: Maybe String
  , endDatetime :: Maybe String
  , assetId :: Maybe String
  }

instance encodeEventRequest :: EncodeJson EventRequest where
  encodeJson (EventRequest ef)
     = "name" := ef.name
    ~> "description" := ef.description
    ~> "start_datetime" := ef.startDatetime
    ~> "end_datetime" := ef.endDatetime
    ~> "asset_id" := ef.assetId
    ~> jsonEmptyObject

initialEventForm =
  { name: ""
  , description: ""
  , startDatetime: ""
  , endDatetime: ""
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
      handleNewEvent Form.Submit = do
        st <- H.get
        H.liftAff $ log $ "blerble"
        -- case res of
        --   Left err ->
        --     H.liftAff $ log $ "invalid " <> err
        --   Right valid_req -> do
        --     H.modify (_ { loading = true })
        --     response <- H.liftAff $ AX.post (apiUrl <> "/admin/events") (encodeJson valid_req)
        --     H.modify (_ { loading = false })
        --     createResponse <- pure $ decodeEventCreateResponse response.response
        --     case createResponse of
        --       Left e -> H.liftAff $ log ("create response error: " <> e)
        --       Right (EventCreateResponse cr ) -> do
        --         -- H.liftAff $ log ("id: " <> show cr.id)
        --         H.liftEff $ flashMessage Success "Created new event"
        --         H.liftEff $ fileUpload "logo_asset" (mkLogoUrl cr.id)
      handleNewEvent (Form.Edit f) =
        H.modify (_form %~ f)

  eval (PostRender next) = do
    H.liftEff $ flatpicker "#start_datetime"
    H.liftEff $ flatpicker "#end_datetime"
    pure next
  eval (SelectEvent _ next) = pure next

mkLogoUrl :: Int -> String
mkLogoUrl i =  apiUrl <> "/admin/events/" <> (show i) <> "/logo"

render :: State -> H.ComponentHTML Input
render st = do
  div [ styleClass "container page" ]
    [ div [ styleClass "row" ]
      [ div [ styleClass "col-lg-12" ]
        [ text (if st.loading then "posting..." else "") ]
        , viewEventForm st.form st.loading
        ]
    ]

viewEventForm f submitted =
  div [ styleClass "admin--form centered"] [
      h3 [] [ text Msg.eventFormHeader ]
    , p_ [ text Msg.eventFormHelp ]
    , div_ $
      Form.renderForm f NewEvent do
        _ <- Form.textField "name" "Name" (_name) noopValidation
        _ <- Form.textField "description" "Description" (_description) noopValidation
        _ <- Form.textField "start_datetime" "Start" _startDatetime  noopValidation
        Form.textField "end_datetime" "End" _endDatetime noopValidation
      -- , Form.fileField "logo_asset" "Logo" f.assetId (const Noop)
        -- Form.submitButton_ Form.Submit
  ]

noopValidation s = Right s
