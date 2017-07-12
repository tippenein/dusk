module Handler.Event where

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Data.Conduit.Binary (sinkLbs)
import Data.Time.Clock as Clock

import qualified Data.Text as T

import Import

getEventR :: Handler Html
getEventR = do
  events <- runDB $ selectList [] [Desc EventStart_time]
  (formWidget, formEnctype) <- generateFormPost eventForm
  defaultLayout $ do
    setTitle "events!"
    $(widgetFile "events")

postEventR :: Handler Html
postEventR = do
  ((result, formWidget), formEnctype) <- runFormPost eventForm
  events <- runDB $ selectList [] [Desc EventName]
  case result of
    FormSuccess (EventForm n d fi)-> do
      filename <- writeToServer fi
      t <- liftIO Clock.getCurrentTime
      _ <- runDB $ insert (Event n d filename True (Just t) Nothing)
      setMessage "Image saved"
      redirect EventR
    _ -> defaultLayout $ do
            setTitle "events!"
            $(widgetFile "events")

writeToServer :: FileInfo -> Handler Text
writeToServer file = do
  filename <- runResourceT $ fileSource file $$ sinkLbs
  let path = imageFilePath $ genFileName filename
  liftIO $ fileMove file path
  return $ T.pack $ genFileName filename
  where
    genFileName lbs = "upload-" ++ base64md5 lbs

data EventForm
  = EventForm
  { ef_name :: Text
  , ef_description :: Maybe Text
  , ef_fileInfo :: FileInfo
  }

eventForm :: Form EventForm
eventForm = renderBootstrap3 BootstrapBasicForm $
  EventForm
    <$> areq textField (textSettings "name") Nothing
    <*> aopt textField (textSettings "description") Nothing
    <*> fileAFormReq "Choose an Event Image"
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
