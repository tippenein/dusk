module FormValidation where
-- https://github.com/slamdata/purescript-halogen/issues/235
import Prelude

import Control.Monad.Except.Trans (ExceptT(..))
import Data.DateTime (DateTime(..))
import Data.Either (Either, either)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Halogen.HTML as H
import Halogen.HTML.Properties as P
import Helper.Format (formatDateTime)

type IProp r i = P.IProp r (i Unit)

-- | A value that can be displayed on a form.  This is the most general type of
-- form value, hence the "G".
--
-- The `e` parameter is the error type.  This is
-- generally the error message that is displayed to the user when the form is
-- invalid. `FormValue` has `e` specialized as `String`.  Most applications
-- will show errors to the end user as `Strings`.
--
-- `a` represents the value in the form.  This will be a `String` for most
-- form values. However, for check boxes, it may be a Boolean.
--
-- `b` represents the final value after it has been validated.
data FormValueG e a b = FormValueG FormValueState (ValidatorG e a b) a

-- | A `FormValueG` with the error type specialized to `String`.
type FormValue a b = FormValueG String a b

-- | A `FormValue` where `a` and `b` are the same thing.  This is similar to
-- the difference between the `Lens` and `LensP` types.
type FormValue' a = FormValue a a

-- | A newtype wrapper around a function that takes an `a` and returns an
-- `Either e b`.  This is the validation function.
--
-- This is similar to a `PrismP a b`.
newtype ValidatorG e a b = ValidatorG (a -> Either e b)

-- | A `ValidatorG` with the error type specialized to `String`.
type Validator a b = ValidatorG String a b

-- | A `Validator` where `a` and `b` are the same type.
type ValidatorP a = Validator a a

-- | The state of the form value.
--
-- `Initial` is a form that hasn't been changed by the user.
--
-- `FormValueChanged` is a form that has been changed by the user.
--
-- This is used by the `formErrorHTML` function.  It doesn't return an error
-- for a form that is in the `Initial` state.  For instance, if a user hasn't
-- entered any text, then the form should not be considered "invalid".
data FormValueState = Initial | FormValueChanged

-- | Turns a `(a -> Either e b)` function into a `Validator`.
validator :: forall e a b . (a -> Either e b) -> ValidatorG e a b
validator = ValidatorG

-- | Take a `Validator` and an initial `a`, and turn it into a `FormValue`.
initFormValue :: forall e a b . ValidatorG e a b -> a -> FormValueG e a b
initFormValue = FormValueG Initial

-- | Update the value `a` that is stored in a `FormValue`.
updateFormValue :: forall e a b . FormValueG e a b -> a -> FormValueG e a b
updateFormValue (FormValueG _ f _) newVal = FormValueG FormValueChanged f newVal

-- | Run a validation for the `FormValue`, returning an `Either` with `Left` of
-- an error message, or `Right` with the resulting value.
validate :: forall e a b . FormValueG e a b -> Either e b
validate (FormValueG _ (ValidatorG p) a) = p a

-- | Just like `validate`, but running in the `ExceptT` monad.
--
-- This can be used to easily do validation in Halogen's `Eval` monad:
--
-- ```purescript
-- eval :: Eval Input State Input (Aff (HalogenEffects (console :: CONSOLE | eff)))
-- eval (FormSubmit next) = do
--     State state <- get
--     runExceptT do
--         email <- validateA state.email
--         password <- validateA state.password
--         lift $ liftAff' $
--             log $ "this line will only get written if email and password are valid."
--     pure next
-- ```
validateA :: forall e m a b . (Applicative m) => FormValueG e a b -> ExceptT e m b
validateA = ExceptT <<< pure <<< validate


-- | Render the string as the "value" HTML property.
--
-- Previously, where you may have had code like this in your ui render function:
--
-- ```purescript
-- H.input [ P.name "email"
--         , P.value (state.email :: String)
--         , ...
--         ]
-- ```
--
-- You can now do something like this:
--
-- ```purescript
-- H.input [ P.name "email"
--         , formValueHTML (state.email :: FormValue String a)
--         , ...
--         ]
-- ```

formValueHTML = P.value <<< rawFormValue
  where
    rawFormValue :: forall e a b . FormValueG e a b -> a
    rawFormValue (FormValueG _ _ a) = a

formValueDateTimeHTML = P.value <<< fromMaybe "" <<< formatDateTime <<< rawFormValue
  where
    rawFormValue :: forall e a b . FormValueG e a b -> a
    rawFormValue (FormValueG _ _ a) = a



-- | Similarly to `formValueHTML`, it takes a `FormValue` and turns it into text.
--
-- Previously, where you may have had code like this in your ui render function:
--
-- ```purescript
-- H.p [ P.class_ $ H.className "form-error" ]
--     [ H.text (state.email :: String) ]
-- ```
--
-- You can now do something like this:
--
-- ```purescript
-- H.p [ P.class_ $ H.className "form-error" ]
--     [ formErrorHTML (state.email :: FormValue a b) ]
-- ```
formErrorHTML = H.text <<< formValueToError
  where
    formValueToError (FormValueG Initial _ _) = mempty
    formValueToError formValue = either id (const mempty) $ validate formValue
