module Handler.Admin.Event where

import           Control.Monad
import           Control.Monad.Trans.AWS
import           Data.Aeson.Types
import           Data.Conduit
import           Data.Conduit.Binary (sinkLbs)
import           Data.Text (Text)
import           Data.Time

import           Import
import qualified S3

getAdminEventR :: Handler Value
getAdminEventR = do
  _ <- requireAuthId
  return $ object [ "status" .= String "ok" ]

postAdminEventLogoR :: Handler ()
postAdminEventLogoR = do
  -- type RequestBodyContents = ([(Text, Text)], [(Text, FileInfo)])
  req <- runRequestBody
  let efs = map snd . snd $ req
  case efs of
    [] -> pure ()
    (file:files) -> do
      filename <- writeToServer file
      sendResponseStatus status201 filename

postAdminEventR :: Handler ()
postAdminEventR = do
  ef <- requireJsonBody :: Handler EventForm
  userId  <- requireAuthId
  let e = eventFormToEvent ef userId
  _ <- runDB $ insert e
  sendResponseStatus status201 ("CREATED" :: Text)

eventFormToEvent :: EventForm -> UserId -> Event
eventFormToEvent EventForm{..} uid = do
  Event {
      eventName = ef_name
    , eventDescription = ef_description
    , eventAsset_id = ef_asset_id
    , eventOwner_id = uid
    , eventAll_day = False
    , eventStart_datetime = ef_eventStartDatetime
    , eventEnd_datetime = ef_eventEndDatetime
    }

fromRight :: Either a b -> Maybe b
fromRight (Right a) = Just a
fromRight (Left _) = Nothing

writeToServer :: FileInfo -> Handler Text
writeToServer file = do
  bucketname <- fmap (appBucketName . appSettings) getYesod
  fileContents <- runResourceT $ fileSource file $$ sinkLbs
  let fn = genFileName fileContents
  _por <- S3.execAWS (
    S3.put bucketname fn fileContents)
  return fn
  where
    genFileName lbs = "logo/upload-" <> (pack $ base64md5 lbs)

data EventForm
  = EventForm
  { ef_name :: Text
  , ef_description :: Maybe Text
  , ef_eventStartDatetime :: Maybe UTCTime
  , ef_eventEndDatetime :: Maybe UTCTime
  , ef_asset_id :: Maybe Text
  }

instance FromJSON EventForm where
  parseJSON (Object v) = do
    -- mimage <- v .: "file_info"
    EventForm
      <$> v .:  "name"
      <*> v .:? "description"
      <*> v .:? "start_time"
      <*> v .:? "end_time"
      <*> v .:? "asset_id"
  parseJSON _ = fail "invalid json object"


-- eventForm :: Form EventForm
-- eventForm = renderBootstrap3 BootstrapBasicForm $
--   EventForm
--     <$> areq textField (textSettings "name" "Your Event's name") Nothing
--     <*> aopt textareaField (textSettings "description" "Describe your event") Nothing
--     <*> aopt textField (daySettings "start") Nothing
--     <*> aopt textField (daySettings "end") Nothing
--     <*> fileAFormReq "Choose an Event Image"
--   where
--     daySettings t = FieldSettings
--           { fsLabel = SomeMessage t
--           , fsTooltip = Nothing
--           , fsId = Just $ "datetimepicker_" <> t <> "_time"
--           , fsName = Nothing
--           , fsAttrs =
--               [ ("class", "form-control")
--               , ("placeholder", "placeholder")
--               ]
--           }
--     textSettings t p = FieldSettings
--           { fsLabel = t
--           , fsTooltip = Nothing
--           , fsId = Nothing
--           , fsName = Nothing
--           , fsAttrs =
--               [ ("class", "form-control")
--               , ("placeholder", p)
--               ]
--           }

parseISO8601 :: Text -> Maybe UTCTime
parseISO8601 = parseTimeM True defaultTimeLocale "%Y-%-m-%-d %H:%M" . unpack

parseDatetime :: Text -> Either FormMessage UTCTime
parseDatetime = maybe (Left MsgInvalidDay) Right . parseISO8601
