module WForm where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Reader.Trans
import Control.Monad.Writer
import Data.Either
import Data.Lens
import Data.Maybe
import Data.Tuple
import Prelude
import Unsafe.Coerce

import Data.Array hiding ((..))
import Data.String as Str
import Halogen (action)
import Halogen.HTML (ClassName(..), PropName(..))
import Halogen.HTML as H
import Halogen.HTML.Core (Prop(..), HTML(..))
import Halogen.HTML.Events as E
import Halogen.HTML.Properties (IProp(..))
import Halogen.HTML.Properties as P
import Helper.Format (unformatDateTime)
import Text.Email.Validate (EmailAddress(..), emailAddress)


-- | ensures the datetime is parseable but leaves it as string for the form
validDateTime :: String -> Either String String
validDateTime dt = case unformatDateTime dt of
  Nothing -> Left "invalid datetime"
  Just e -> Right dt


optional :: String -> Either String String
optional a = Right a

nonBlank :: String -> Either String String
nonBlank a = case Str.length a of
                0 -> Left "can't be blank"
                _ -> Right a

emailValidator :: String -> Either String String
emailValidator "" = Right ""
emailValidator email = case emailAddress email of
      Nothing -> Left "invalid email"
      Just e -> Right email

type WForm v f a =
  ReaderT
    (Tuple a (FormAction f a))
    -- TODO: Tuple of arrays of errors and HTMLs
    (Writer (Array (HTML v (f Unit))))
    a

type FormError = Array String
type FormAction f a = FormInput a -> Unit -> f Unit
data FormInput a
  = Submit
  | Edit (a -> a)


renderForm
  :: forall a v f
   . a
  -> FormAction f a
  -> WForm v f a
  -> Array (HTML v (f Unit))
renderForm _data eventType fields =
  execWriter (runReaderT fields (Tuple _data eventType)) <> [(submitButton_ (eventType Submit))]

emailField = field P.InputEmail
textField = field P.InputText
passwordField = field P.InputPassword
dateField = field P.InputDate
fileField = field P.InputFile

emailFieldOpt = fieldOpt P.InputEmail
textFieldOpt = fieldOpt P.InputText
passwordFieldOpt = fieldOpt P.InputPassword
dateFieldOpt = fieldOpt P.InputDate
fileFieldOpt = fieldOpt P.InputFile

fieldOpt
  :: forall a b f v
   . P.InputType
  -> String
  -> String
  -> Lens' a (Maybe String)
  -> (String -> Either String String)
  -> WForm v f a
fieldOpt inpType id_ label lens_ validator = do
    Tuple data_ eventType <- ask
    let item = data_ ^. lens_
        validation = case item of
                          Nothing -> Right ""
                          Just i -> validator i
        classes = case validation of
                       Left _ -> [ ClassName "form-group has-error" ]
                       Right _ -> [ ClassName "form-group" ]
        errMsg = case validation of
                      Left str -> str
                      Right _ -> ""
    tell [html eventType errMsg classes item]
    pure (unsafeCoerce item)
  where
    html :: FormAction f a -> String -> Array ClassName -> (Maybe String) -> HTML v (f Unit)
    html eventType errMsg classes item =
      H.div [ P.classes classes ]
        [ H.label [ P.for id_ ] [ H.text label ]
        , H.input
          [ P.id_ id_
          , P.classes [ ClassName "form-control" ]
          , P.prop (PropName "type") inpType
          , P.value $ fromMaybe "" item
          , E.onValueChange (E.input (\a -> eventType (Edit (\b -> set lens_ (stringToMaybe a) b))))
          ]
        , H.span_ [ H.text errMsg ]
        ]

stringToMaybe "" = Nothing
stringToMaybe a = Just a

field
  :: forall a b f v
   . P.InputType
  -> String
  -> String
  -> Lens' a String
  -> (String -> Either String String)
  -> WForm v f a
field inpType id_ label lens_ validator = do
    Tuple data_ eventType <- ask
    let item = data_ ^. lens_
        validation = validator item
        classes = case validation of
                       Left _ -> [ ClassName "form-group has-error" ]
                       Right _ -> [ ClassName "form-group" ]
        errMsg = case validation of
                      Left str -> str
                      Right _ -> ""
    tell [html data_ eventType errMsg classes item]
    -- TODO: handle this better
    pure (unsafeCoerce item)
  where
    html :: a -> FormAction f a -> String -> Array ClassName -> String -> HTML v (f Unit)
    html data_ eventType errMsg classes item =
      H.div [ P.classes classes ]
        [ H.label [ P.for id_ ] [ H.text label ]
        , H.input
          [ P.id_ id_
          , P.classes [ ClassName "form-control" ]
          , P.prop (PropName "type") inpType
          , P.value item
          , E.onValueChange (E.input (\a -> eventType (Edit (\b -> set lens_ a b))))
          ]
        , H.span_ [ H.text errMsg ]
        ]

styleClass = P.class_ <<< H.ClassName

submitButton t h =
  H.button
  -- TODO fix no propogate and preventDefault
    [ E.onClick (E.input_ h)
    , styleClass "btn btn-primary pull-right"
    , P.type_ P.ButtonSubmit ]
    [ H.text t ]

submitButton_ = submitButton "Submit"
