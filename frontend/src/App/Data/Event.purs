module App.Data.Event where

import Prelude
import Data.Either (Either)
import Data.Maybe
import Data.DateTime
import Data.JSDate

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, fromArray, jsonEmptyObject, (.?), (:=), (~>))

newtype Event = Event
  { id :: Int
  , name :: String
  , description :: String
  , asset_id    :: String
  , owner_id    :: Int
  , all_day     :: Boolean
  -- , start_datetime :: DateTime
  -- , end_datetime   :: Maybe DateTime
  }

-- parseISO8601 :: forall eff. String -> Eff (locale :: LOCALE | eff) Maybe DateTime
parseISO8601 p = toDateTime <$> parse p

instance decodeJsonEvent :: DecodeJson Event where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    name <- obj .? "name"
    description <- obj .? "description"
    asset_id <- obj .? "asset_id"
    owner_id <- obj .? "owner_id"
    all_day <- obj .? "all_day"
    -- start_datetime <- parseISO8601 <$> obj .? "start_datetime"
    -- end_datetime <- parseISO8601 <$> obj .? "end_datetime"
    pure $ Event {
        id
      , name
      , description
      , asset_id
      , owner_id
      , all_day
      -- , start_datetime
      -- , end_datetime
      }

instance encodeJsonEvent :: EncodeJson Event where
  encodeJson (Event event)
     = "id" := event.id
    ~> "name" := event.name
    ~> "description" := event.description
    ~> "asset_id" := event.asset_id
    ~> "owner_id" := event.owner_id
    ~> "all_day" := event.all_day
    -- ~> "start_datetime" := event.start_datetime
    -- ~> "end_datetime" := event.end_datetime
    ~> jsonEmptyObject


newtype Events = Events { events :: Array Event }

instance decodeJsonEvents :: DecodeJson Events where
  decodeJson json = do
    obj <- decodeJson json
    events <- obj .? "events"
    pure $ Events { events }

instance encodeJsonEvents :: EncodeJson Events where
  encodeJson (Events events)
     = "events" := events.events
    ~> jsonEmptyObject


decodeEvent :: Json -> Either String Event
decodeEvent = decodeJson

decodeEvents :: Json -> Either String Events
decodeEvents = decodeJson
