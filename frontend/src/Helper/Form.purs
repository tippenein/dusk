module Helper.Form where

import FormValidation (Validator, formErrorHTML, formValueHTML, validator)
import Halogen.HTML.Properties (ButtonType(..))
import Halogen.HTML hiding (map)
import Halogen.HTML.Elements as Elements
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as HP
import Helper (styleClass)
import Import hiding (div)
import Text.Email.Validate (EmailAddress(..), emailAddress)


nonBlank :: Validator String String
nonBlank = validator go
  where
    go :: String -> Either String String
    go "" = Left "can't be blank"
    go a  = Right a

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

formSubmit label action =
  button [ HP.type_ ButtonSubmit
         , styleClass "btn btn-default"
         , E.onClick (E.input_ action)
         ]
         [ text label ]
