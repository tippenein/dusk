module Handler.AdminEvent where

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Data.Conduit.Binary (sinkLbs)
import Data.Time.Clock as Clock

import qualified Data.Text as T

import Import

getAdminEventR :: Handler Html
getAdminEventR = do
  events <- runDB $ selectList [] [Desc EventStart_time]
  (formWidget, _formEnctype) <- generateFormPost eventForm
  defaultLayout $ do
    setTitle "events!"
    $(widgetFile "events")

postAdminEventR :: Handler Html
postAdminEventR = do
  ((result, formWidget), _formEnctype) <- runFormPost eventForm
  events <- runDB $ selectList [] [Desc EventName]
  case result of
    FormSuccess (EventForm n d fi)-> do
      filename <- writeToServer fi
      t <- liftIO Clock.getCurrentTime
      _ <- runDB $ insert (Event n d filename True (Just t) Nothing)
      setMessage "Event saved"
      defaultLayout $ do
        [whamlet|
<div .container>
  <div .row>
    <div .col-lg-12>
      <div .bs-callout bs-callout-info well>
        <form .form-horizontal method=post action=@{EventR}#forms>
          ^{formWidget}

          <button .btn.btn-primary type="submit">
            Post it!
|]
    FormFailure reasons -> defaultLayout $ do
      setMessage $ toHtml $ unlines reasons
    _ -> defaultLayout $ do
      setMessage "something went wrong"


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
