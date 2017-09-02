module Helper.Form where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.DateTime (DateTime(..))
import Data.String (length)
import FormValidation (Validator, formErrorHTML, formValueDateTimeHTML, formValueHTML, validator)
import Halogen.HTML hiding (map)
import Halogen.HTML.Elements as Elements
import Halogen.HTML.Events as E
import Halogen.HTML.Properties (ButtonType(..))
import Halogen.HTML.Properties as HP
import Helper (styleClass)
import Helper.Flatpicker as Flatpicker
import Helper.Format (unformatDateTime)
import Import hiding (div)
import Text.Email.Validate (EmailAddress(..), emailAddress)
import Top.Monad (TopEffects)


validDateTime :: Validator String String
validDateTime = validator go
  where
    go :: String -> Either String String
    go dt = case unformatDateTime dt of
      Nothing -> Left "invalid datetime"
      Just e -> Right dt

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


formSubmit label action submitting =
  button [ HP.type_ ButtonSubmit
         , styleClass "btn btn-default"
         , HP.prop (PropName "disabled") if submitting then "disabled" else ""
         , E.onClick (E.input_ action)
         ]
         [ text label ]

flatpicker :: String -> Eff TopEffects Unit
flatpicker s = Flatpicker.flatpicker(s)
