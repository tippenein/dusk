module Handler.Admin.Event where

import           Control.Monad
import           Control.Monad.Trans.AWS
import           Data.Aeson.Types
import           Data.Conduit
import           Data.Conduit.Binary (sinkLbs)
import           Data.Text (Text)
import           Data.Time
import           Database.Persist.Sql
import           App.Crud (CreateResponse(..))
import           App.Form

import           Import
import qualified S3

getAdminEventR :: Handler Value
getAdminEventR = do
  _ <- requireAuthId
  return $ object [ "status" .= String "ok" ]

postAdminEventLogoR :: Key Event -> Handler ()
postAdminEventLogoR event_id = do
  -- type RequestBodyContents = ([(Text, Text)], [(Text, FileInfo)])
  req <- runRequestBody
  let efs = map snd . snd $ req
  case efs of
    [] -> pure ()
    (file:_files) -> do
      filename <- writeToServer file
      _ <- runDB $ updateGet event_id [EventAsset_id =. (Just filename)]
      sendResponseStatus status201 ("CREATED" :: Text)

postAdminEventR :: Handler Value
postAdminEventR = do
  ef <- requireJsonBody :: Handler EventForm
  userId  <- requireAuthId
  let e = eventFormToEvent ef userId
  case e of
    Left err ->
      return $ toJSON $ CreateFailure err
    Right ev -> do
      i <- runDB $ insert ev
      return $ toJSON $ CreateSuccess (fromSqlKey i)

eventFormToEvent :: EventForm -> UserId -> Either Text Event
eventFormToEvent EventForm{..} uid = do
    mstart <- parseDateTimeM ef_startDatetime
    mend <- parseDateTimeM ef_endDatetime
    pure $ Event {
        eventName = ef_name
      , eventDescription = ef_description
      , eventAsset_id = Nothing -- | XXX We upload this separately
      , eventOwner_id = uid
      , eventAll_day = False
      , eventStart_datetime = mstart
      , eventEnd_datetime =  mend
      }

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

-- instance FromJSON EventForm where
--   parseJSON (Object v) = do
--     mstart <- v .:? "start_datetime"
--     mend <- v .:? "end_datetime"
--     EventForm
--       <$> v .:  "name"
--       <*> v .:? "description"
--       <*> decodeMaybeDT mstart
--       <*> decodeMaybeDT mend
--       <*> v .:? "asset_id"
--   parseJSON _ = fail "invalid json object"


decodeMaybeDT :: (Monad m) => Maybe Text -> m (Maybe UTCTime)
decodeMaybeDT (Just dt) = pure $ parseISO8601 dt
decodeMaybeDT Nothing = pure Nothing

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

parseDateTime :: Text -> Either Text UTCTime
parseDateTime = maybe (Left "invalid datetime") Right . parseISO8601

parseDateTimeM :: Maybe Text -> Either Text (Maybe UTCTime)
parseDateTimeM (Just dt) =
  case parseISO8601 dt of
    Just s -> Right $ Just s
    Nothing -> Left $ "invalid datetime: " <> dt
parseDateTimeM Nothing = Right Nothing
