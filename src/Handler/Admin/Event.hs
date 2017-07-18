module Handler.Admin.Event where

import           Data.Conduit.Binary (sinkLbs)
import           Data.Time.Calendar
import           Data.Time.Clock as Clock
import           Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

import qualified Data.Text as T

import           Import

getAdminEventR :: Handler Html
getAdminEventR = do
  (formWidget, _formEnctype) <- generateFormPost eventForm
  defaultLayout $ do
    setTitle' "Create Event"
    [whamlet|
<div .container>
  <div .row>
    <div .col-lg-6>
      <div .bs-callout bs-callout-info well>
        <form .form-horizontal method=post action=@{AdminEventR}#forms>
          ^{formWidget}

          <button .btn.btn-primary type="submit">
            Post it!
    <div .col-lg-6>
|]

postAdminEventR :: Handler Html
postAdminEventR = do
  ((result, formWidget), _formEnctype) <- runFormPost eventForm
  (userId, _user) <- requireAuthPair
  case result of
    FormSuccess (EventForm n d day fi)-> do
      filename <- writeToServer fi
      -- oh gawd wtf
      _ <- runDB $ insert (Event n (fmap unTextarea d) filename userId day Nothing (Just (UTCTime day 0)) Nothing)
      setMessage "Event saved"
      defaultLayout $ do
        [whamlet|
<div .container>
  <div .row>
    <div .col-lg-12>
      <div .bs-callout bs-callout-info well>
        <form .form-horizontal method=post action=@{AdminEventR}#forms>
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
  , ef_description :: Maybe Textarea
  , ef_eventStartDay :: Day
  , ef_fileInfo :: FileInfo
  }

eventForm :: Form EventForm
eventForm = renderBootstrap3 BootstrapBasicForm $
  EventForm
    <$> areq textField (textSettings "name" "Your Event's name") Nothing
    <*> aopt textareaField (textSettings "description" "Describe your event") Nothing
    <*> areq dayField (daySettings "start day") Nothing
    <*> fileAFormReq "Choose an Event Image"
  where
    daySettings t = FieldSettings
          { fsLabel = t
          , fsTooltip = Nothing
          , fsId = Nothing
          , fsName = Nothing
          , fsAttrs =
              [ ("class", "form-control")
              , ("placeholder", "placeholder")
              ]
          }
    textSettings t p = FieldSettings
          { fsLabel = t
          , fsTooltip = Nothing
          , fsId = Nothing
          , fsName = Nothing
          , fsAttrs =
              [ ("class", "form-control")
              , ("placeholder", p)
              ]
          }
