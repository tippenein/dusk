module App.Data.Event where

import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Either (hush, Either)
import Data.Maybe (Maybe)
import Data.Formatter.DateTime as FD
import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, jsonEmptyObject, (.?), (:=), (~>))

newtype Event = Event
  { id :: Int
  , name :: String
  , description :: String
  , asset_id    :: String
  , owner_id    :: Int
  , all_day     :: Boolean
  , start_datetime :: Maybe DateTime
  , end_datetime   :: Maybe DateTime
  }

instance decodeJsonEvent :: DecodeJson Event where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    name <- obj .? "name"
    description <- obj .? "description"
    asset_id <- obj .? "asset_id"
    owner_id <- obj .? "owner_id"
    all_day <- obj .? "all_day"
    start_datetime <- unformatDateTime <$> (obj .? "start_datetime")
    end_datetime <- unformatDateTime <$> (obj .? "end_datetime")
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

instance encodeEvent :: EncodeJson Event where
  encodeJson (Event event)
     = "id" := event.id
    ~> "name" := event.name
    ~> "description" := event.description
    ~> "asset_id" := event.asset_id
    ~> "owner_id" := event.owner_id
    ~> "all_day" := event.all_day
    ~> "start_datetime" := (formatDateTime <$> event.start_datetime)
    ~> "end_datetime" := (formatDateTime <$> event.end_datetime)
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

decodeEvent :: Json -> Either String Event
decodeEvent = decodeJson

decodeEvents :: Json -> Either String Events
decodeEvents = decodeJson

formatDateTime :: DateTime.DateTime -> Maybe String
formatDateTime x = hush $ FD.formatDateTime dtformat x

dtformat :: String
dtformat = "YYYY-MM-DDTHH:mmZ"

unformatDateTime :: String -> Maybe DateTime.DateTime
unformatDateTime x = hush $ FD.unformatDateTime dtformat x

formatDate :: DateTime.Date -> Maybe String
formatDate x = formatDateTime $ DateTime.DateTime x bottom

unformatDate :: String -> Maybe DateTime.Date
unformatDate x = map DateTime.date $ unformatDateTime x

formatTime :: DateTime.Time -> Maybe String
formatTime x = hush $ FD.formatDateTime "HH:mm:ss.SSS" $ DateTime.DateTime bottom x

unformatTime :: String -> Maybe DateTime.Time
unformatTime x = hush $ map DateTime.time $ FD.unformatDateTime "HH:mm:ss.SSS" x
