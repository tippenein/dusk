module Component.Admin.Event where


import Helper

import App.Data.Event as Event
import Control.Monad.Aff (Aff)
import DOM.Event.Event (preventDefault)
import DOM.Event.Types as DOM
import Data.DateTime as DateTime
import Data.Formatter.DateTime as FD
import FormValidation (FormValue, formValueHTML, initFormValue, updateFormValue, validateA)
import Halogen as H
import Halogen.HTML hiding (map)
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as HP
import Helper.Form as Form
import Import hiding (div)
import Network.HTTP.Affjax as AX
import Top.Monad (Top)

data Slot = Slot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

parseDateTime x = hush $ FD.unformatDateTime "AA, MMM D" x

type EventForm =
  { name :: FormValue' String
  , description :: FormValue' String
  }

initialEventForm =
  { name: initFormValue Form.nonBlank ""
  , description: initFormValue Form.nonBlank ""
  }
type FormValue' a = FormValue String a

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
  | SelectEvent Int a

ui :: H.Component HTML Input Unit Void Top
ui =
  H.component
    { initialState: const initialState
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
    -- response <- H.liftAff $ AX.post (apiUrl <> "/admin/events") (toPostData form)
    -- es <- pure $ Event.decodeCreateResponse response.response
    -- case es of
    --   Left e ->
    --     H.modify (_ { error = Just e, loading = false, form = Nothing})
    --   Right (Event.CreateResponse {event: res}) ->
    --     H.modify (_ { error = Nothing, loading = false, form = res})
    state <- H.get
    name <- runExceptT $ validateA state.form.name
    pure next
  eval (UpdateName name next) = do
    H.modify (\st -> st { form = st.form { name = updateFormValue st.form.name name } })
    pure next
  eval (UpdateDescription description next) = do
    H.modify (\st -> st { form = st.form { description = updateFormValue st.form.description description } })
    pure next
  eval (SelectEvent _ next) = pure next

render :: State -> H.ComponentHTML Input
render st =
  div [ styleClass "container page" ]
    [ div [ styleClass "row" ]
      [ div [ styleClass "col-lg-12" ]
        [ text (if st.loading then "posting..." else "") ]
        , viewEventForm st.form
        ]
    ]

viewEventForm st =
  form [ E.onSubmit (E.input PreventDefault) ]
    [ Form.simpleTextInput st.name "name" "Name" UpdateName
    , Form.simpleTextAreaInput st.description "description" "Description" UpdateDescription
    , Form.formSubmit "Submit" FormSubmit
    ]
