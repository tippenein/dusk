module Handler.Event where

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Data.Conduit.Binary (sinkLbs)

import Import

getEventR :: Handler Html
getEventR = do
  events <- runDB $ selectList [] [Desc EventName]
  (formWidget, formEnctype) <- generateFormPost eventForm
  defaultLayout $ do
    setTitle "events!"
    $(widgetFile "events")

postEventR :: Handler Html
postEventR = do
  ((result, formWidget), formEnctype) <- runFormPost eventForm
  events <- runDB $ selectList [] [Desc EventName]
  -- let fi = fileSource result
  -- bytes <- toStrict . runResourceT $ fileSource fi $$ sinkLbs
  case formToEvent <$> result of
    FormSuccess res -> do
      entryId <- runDB $ insert res
      redirect EventR
    _ -> defaultLayout $ do
            setTitle "events!"
            $(widgetFile "events")


formToEvent :: EventForm -> Event
formToEvent (EventForm name maybe_descrip fi) = do
  -- do something with the FileInfo
  Event name maybe_descrip "derp"

data EventForm = EventForm Text (Maybe Text) FileInfo

eventForm :: Form EventForm
eventForm = renderBootstrap3 BootstrapBasicForm $
  EventForm
    <$> areq textField (textSettings "name") Nothing
    <*> aopt textField (textSettings "description") Nothing
    <*> fileAFormReq "Choose a file"
  where
    textSettings t = FieldSettings
          { fsLabel = t
          , fsTooltip = Nothing
          , fsId = Nothing
          , fsName = Nothing
          , fsAttrs =
              [ ("class", "form-control")
              , ("placeholder", "placeholder")
              ]
          }
