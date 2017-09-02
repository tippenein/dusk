module Component.Admin.Event where


import Component.Admin.Event.Form
import Helper

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import DOM.Event.Event (preventDefault)
import DOM.Event.Types as DOM
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Argonaut.Encode (encodeJson)
import Data.DateTime (DateTime(..))
import Data.DateTime as DateTime
import Data.Formatter.DateTime as FD
import Data.Generic (gShow)
import Data.Lens (Lens', lens, (%~))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
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
import Network.HTTP.Affjax as AX
import Top.Monad (Top)

data Slot = Slot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

_form :: Lens' State EventForm
_form = lens _.form _ { form = _ }

newtype EventRequest = EventRequest
  { name :: String
  , description :: String
  , startDatetime :: String
  , endDatetime :: String
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

-- encodeEventRequest :: 
-- encodeEventRequest = encodeJson

initialEventForm =
  { name: initFormValue Form.nonBlank ""
  , description: initFormValue Form.nonBlank ""
  , startDatetime: initFormValue Form.validDateTime ""
  , endDatetime: initFormValue Form.validDateTime ""
  }

type State =
  { loading :: Boolean
  , form :: EventForm
  , error :: Maybe String
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
  initialState = { loading: false, form: initialEventForm, error: Nothing }

  eval :: Input ~> H.ComponentDSL State Input Void Top
  eval (Noop next) = pure next
  eval (PreventDefault e next) = H.liftEff (preventDefault e) $> next

  eval (FormSubmit next) = do
    st <- H.get
    -- response <- H.liftAff $ AX.post (apiUrl <> "/admin/events/logo") (st.fileForm)
    res <- runExceptT $ do
      name <- validateA st.form.name
      description <- validateA st.form.description
      startDatetime <- validateA st.form.startDatetime
      endDatetime <- validateA st.form.endDatetime
      -- let er = { name = name, description = description, startDatetime = startDatetime, endDatetime = endDatetime, assetId = Nothing}
      let er = EventRequest {name: name, description: description, startDatetime: startDatetime, endDatetime: endDatetime, assetId: Nothing}
      H.modify (_ { loading = true })
      response <- H.liftAff $ AX.post (apiUrl <> "/admin/events") (encodeJson er)
      H.modify (_ { loading = false })
      H.liftAff $ log $ "derp" <> response.response
      H.liftEff $ flashMessage Success "created new event"
    -- es <- pure $ Event.decodeCreateResponse response.response
    -- case es of
    --   Left e ->
    --     H.modify (_ { error = Just e, loading = false, form = Nothing})
    --   Right (Event.CreateResponse {event: res}) ->
    --     H.modify (_ { error = Nothing, loading = false, form = res})
    -- name <- runExceptT $ validateA state.form.name
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

render :: State -> H.ComponentHTML Input
render st = do
  div [ styleClass "container page" ]
    [ div [ styleClass "row" ]
      [ div [ styleClass "col-lg-12" ]
        [ text (if st.loading then "posting..." else "") ]
        , viewEventForm st.form st.loading
        ]
    ]

viewEventForm f submitted= do
  form [ E.onSubmit (E.input PreventDefault) ]
    [ Form.simpleTextInput f.name "name" "Name" UpdateName
    , Form.simpleTextAreaInput f.description "description" "Description" UpdateDescription
    , div [ HP.id_ "start_datetime" ]
      [ Form.simpleTextInput f.startDatetime "start_datetime" "Start" UpdateStart ]
    , div [ HP.id_ "end_datetime" ]
      [ Form.simpleTextInput f.endDatetime "end_datetime" "End" UpdateEnd ]
    , Form.formSubmit "Submit" FormSubmit submitted
    ]
