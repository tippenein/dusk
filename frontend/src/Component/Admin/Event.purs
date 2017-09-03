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
import Helper.Form as Form
import Helper.Format (formatDateTime, unformatDateTime)
import Import hiding (div)
import Message as Msg
import Network.HTTP.Affjax as AX
import Top.Monad (Top)

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
  { name: initFormValue Form.nonBlank ""
  , description: initFormValue Form.nonBlank ""
  , startDatetime: initFormValue Form.validDateTime ""
  , endDatetime: initFormValue Form.validDateTime ""
  }

type State =
  { loading :: Boolean
  , form :: EventForm
  }

data Input a
  = Noop a
  | PreventDefault DOM.Event a
  | FormSubmit a
  | UpdateName String a
  | UpdateDescription String a
  | UpdateStart String a
  | UpdateEnd String a
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

  eval (FormSubmit next) = do
    st <- H.get
    res <- runExceptT $ do
      name <- validateA st.form.name
      description <- validateA st.form.description
      startDatetime <- validateA st.form.startDatetime
      endDatetime <- validateA st.form.endDatetime
      pure $ EventRequest {
          name: name
        , description: stringToMaybe description
        , startDatetime: stringToMaybe startDatetime
        , endDatetime: stringToMaybe endDatetime
        , assetId: Nothing }
    case res of
      Left err ->
        H.liftAff $ log $ "invalid " <> err
      Right valid_req -> do
        H.modify (_ { loading = true })
        response <- H.liftAff $ AX.post (apiUrl <> "/admin/events") (encodeJson valid_req)
        H.modify (_ { loading = false })
        createResponse <- pure $ decodeEventCreateResponse response.response
        case createResponse of
          Left e -> H.liftAff $ log ("create response error: " <> e)
          Right (EventCreateResponse cr ) -> do
            -- H.liftAff $ log ("id: " <> show cr.id)
            H.liftEff $ flashMessage Success "Created new event"
            H.liftEff $ fileUpload "logo_asset" (mkLogoUrl cr.id)
    pure next
  eval (PostRender next) = do
    H.liftEff $ Form.flatpicker "#ff-start_datetime"
    H.liftEff $ Form.flatpicker "#ff-end_datetime"
    pure next
  eval (UpdateName name next) = do
    H.modify $ ((_form <<< _name) %~ (\f -> updateFormValue f name))
    pure next
  eval (UpdateDescription description next) = do
    H.modify $ ((_form <<< _description) %~ (\f -> updateFormValue f description))
    pure next
  eval (UpdateStart start_datetime next) = do
    H.modify $ ((_form <<< _startDatetime) %~ (\f -> updateFormValue f start_datetime))
    pure next
  eval (UpdateEnd end_datetime next) = do
    H.modify $ ((_form <<< _endDatetime) %~ (\f -> updateFormValue f end_datetime))
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

stringToMaybe :: String -> Maybe String
stringToMaybe "" = Nothing
stringToMaybe a = Just a

viewEventForm f submitted =
  div [ styleClass "admin--form centered"] [
      h3 [] [ text Msg.eventFormHeader ]
    , p_ [ text Msg.eventFormHelp ]
    , form [ E.onSubmit (E.input PreventDefault) ]
      [ Form.simpleTextInput f.name "name" "Name" UpdateName
      , Form.simpleTextAreaInput f.description "description" "Description" UpdateDescription
      , div [ HP.id_ "start_datetime" ]
        [ Form.simpleTextInput f.startDatetime "start_datetime" "Start" UpdateStart ]
      , div [ HP.id_ "end_datetime" ]
        [ Form.simpleTextInput f.endDatetime "end_datetime" "End" UpdateEnd ]
      , Form.simpleFileInput "logo_asset" "Logo"
      , Form.formSubmit "Submit" FormSubmit submitted
      ]
    ]
