module App.Data.Event where

import Import

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonEmptyObject, toObject, (.?), (:=), (~>))
import Data.Argonaut.Core (isNull)
import Data.Formatter.DateTime as FD
import Data.StrMap (StrMap)
import Data.StrMap as SM


data CreateResponse e a = Failure e | Success (Tuple Int a)

infix 7 getFieldOptionalNull as .??

-- | Either it's there and null, or not there at all, either way it's Nothing
getFieldOptionalNull :: forall a. (DecodeJson a) => StrMap Json -> String -> Either String (Maybe a)
getFieldOptionalNull strMap s =
  case SM.lookup s strMap of
    Nothing -> pure Nothing
    Just v -> checkNull v
  where
    checkNull j
      | isNull j = pure Nothing
      | otherwise = decodeJson j

newtype Event = Event
  { id :: Int
  , name :: String
  , description :: Maybe String
  , asset_id    :: Maybe String
  , owner_id    :: Int
  , all_day     :: Boolean
  , start_datetime :: Maybe String
  , end_datetime   :: Maybe String
  }

derive instance eqEvent :: Eq Event
derive instance genericEvent :: Generic Event _

instance decodeJsonEvent :: DecodeJson Event where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    name <- obj .? "name"
    description <- obj .?? "description"
    asset_id <- obj .?? "asset_id"
    owner_id <- obj .? "owner_id"
    all_day <- obj .? "all_day"
    start_datetime <- obj .?? "start_datetime"
    end_datetime <- obj .?? "end_datetime"
    pure $ Event {
        id
      , name
      , description
      , asset_id
      , owner_id
      , all_day
      , start_datetime
      , end_datetime
      }

instance encodeJsonEvent :: EncodeJson Event where
  encodeJson (Event event)
     = "id" := event.id
    ~> "name" := event.name
    ~> "description" := event.description
    ~> "asset_id" := event.asset_id
    ~> "owner_id" := event.owner_id
    ~> "all_day" := event.all_day
    ~> "start_datetime" := event.start_datetime
    ~> "end_datetime" := event.end_datetime
    ~> jsonEmptyObject


newtype Events = Events { events :: Array Event }

instance decodeJsonEvents :: DecodeJson Events where
  decodeJson json = do
    obj <- decodeJson json
    events <- obj .? "events"
    pure $ Events { events }

instance encodeEvents :: EncodeJson Events where
  encodeJson (Events events)
     = "events" := events.events
    ~> jsonEmptyObject

encodeEvent :: Event -> Json
encodeEvent = encodeJson

decodeEvent :: Json -> Either String Event
decodeEvent = decodeJson

decodeEvents :: Json -> Either String Events
decodeEvents = decodeJson
