module App.Data.Event where

import Prelude
import Data.Either (Either)

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, fromArray, jsonEmptyObject, (.?), (:=), (~>))

newtype Event = Event
  { id :: Int
  , name :: String
  , description :: String
  }

instance decodeJsonEvent :: DecodeJson Event where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    name <- obj .? "name"
    description <- obj .? "description"
    pure $ Event { id, name, description }

instance encodeJsonEvent :: EncodeJson Event where
  encodeJson (Event event)
     = "id" := event.id
    ~> "name" := event.name
    ~> "description" := event.description
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
