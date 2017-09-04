module Helper.Form where

import Control.Monad.Eff (Eff)
import Data.String (length)
import FormValidation (Validator, formErrorHTML, formValueDateTimeHTML, formValueHTML, validator)
import Halogen.HTML hiding (map)
import Halogen.HTML.Elements as Elements
import Halogen.HTML.Events as E
import Halogen.HTML.Properties (ButtonType(..), InputType(..))
import Halogen.HTML.Properties as HP
import Helper (stringToMaybe, styleClass)
import Helper.Flatpicker as Flatpicker
import Helper.Format (unformatDateTime)
import Import hiding (div)
import Text.Email.Validate (EmailAddress, emailAddress)
import Top.Monad (TopEffects)


-- | same as validDateTime but doesn't fail on empty
validDateTimeOpt :: Validator String String
validDateTimeOpt = validator go
  where
    go :: String -> Either String String
    go "" = Right ""
    go dt = case unformatDateTime dt of
      Nothing -> Left "invalid datetime"
      Just e -> Right dt

-- | ensures the datetime is parseable but leaves it as string for the form
validDateTimeReq :: Validator String String
validDateTimeReq = validator go
  where
    go :: String -> Either String String
    go dt = case unformatDateTime dt of
      Nothing -> Left "invalid datetime"
      Just e -> Right dt

fieldToMaybe :: Validator String (Maybe String)
fieldToMaybe = validator go
  where
    go :: String -> Either String (Maybe String)
    go a = pure $ stringToMaybe a


allValid :: Validator String String
allValid = validator go
  where
    go a = pure a

nonBlank :: Validator String String
nonBlank = validator go
  where
    go :: String -> Either String String
    go a = case length a of
                0 -> Left "can't be blank"
                _ -> Right a

emailValidator :: Validator String EmailAddress
emailValidator = validator go
  where
    go :: String -> Either String EmailAddress
    go email = case emailAddress email of
      Nothing -> Left "invalid email"
      Just e -> Right e

simpleTextInput accessor field name action =
  genericInput "text" accessor field name action

simpleTextAreaInput accessor field name action =
  genericInput "textarea" accessor field name action

genericInput typ accessor field name action =
  div [ styleClass "row" ]
    [ div [ styleClass "form-group" ]
      [ label [ HP.for $ "ff-" <> field ] [ text name ] ]
      , Elements.input [
           styleClass "form-control"
         , HP.name field
         , HP.id_ $ "ff-" <> field
         , formValueHTML accessor
         , E.onValueChange (E.input action)
        ]
      , div [ ]
        [ p [ styleClass "form-error" ]
          [ formErrorHTML accessor ]
        ]
    ]

simpleDatetimeInput accessor field name action =
  div [ styleClass "row" ]
    [ div [ styleClass "form-group" ]
      [ label [ HP.for $ "ff-" <> field ] [ text name ] ]
      , Elements.input [
           styleClass "form-control"
         , HP.name field
         , HP.id_ $ "ff-" <> field
         , formValueDateTimeHTML accessor
         , E.onValueChange (E.input action)
        ]
      , div [ ]
        [ p [ styleClass "form-error" ]
          [ formErrorHTML accessor ]
        ]
    ]


simpleFileInput field name =
  div [ styleClass "row" ]
    [ div [ styleClass "form-group" ]
      [ label [ HP.for field ] [ text name ] ]
      , Elements.input [
           styleClass "form-control"
         , HP.type_ InputFile
         , HP.name field
         , HP.id_ field
        ]
    ]

formSubmit label action submitting =
  button [ HP.type_ ButtonSubmit
         , styleClass "btn btn-default"
         , HP.prop (PropName "disabled") if submitting then "disabled" else ""
         , E.onClick (E.input_ action)
         ]
         [ text label ]

flatpicker :: String -> Eff TopEffects Unit
flatpicker s = Flatpicker.flatpicker(s)
