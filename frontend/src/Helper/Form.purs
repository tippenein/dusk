module Helper.Form where

import FormValidation (Validator, formErrorHTML, formValueHTML, validator)
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
  div [ styleClass "row" ]
    [ div [ styleClass "col-md-4" ]
      [ label [ HP.for field ] [ text name ] ]
    , div [ styleClass "col-md-4" ]
      [ Elements.input [ styleClass "form-control"
                 , HP.name field
                 , formValueHTML accessor
                 , E.onValueChange (E.input action)
              ]
      ]
    , div [ styleClass "col-md-4" ]
      [ p [ styleClass "form-error" ]
            [ formErrorHTML accessor ]
      ]
    ]
