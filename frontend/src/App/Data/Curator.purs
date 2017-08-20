module App.Data.Curator where

import Import

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, jsonEmptyObject, (.?), (:=), (~>))

newtype Curator = Curator
  { id :: Int
  , name :: String
  }

instance decodeJsonCurator :: DecodeJson Curator where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    name <- obj .? "name"
    pure $ Curator {
        id
      , name
      }

instance encodeCurator :: EncodeJson Curator where
  encodeJson (Curator curator)
     = "id" := curator.id
    ~> "name" := curator.name
    ~> jsonEmptyObject


newtype Curators = Curators { curators :: Array Curator }

instance decodeJsonCurators :: DecodeJson Curators where
  decodeJson json = do
    obj <- decodeJson json
    curators <- obj .? "curators"
    pure $ Curators { curators }

instance encodeCurators :: EncodeJson Curators where
  encodeJson (Curators curators)
     = "curators" := curators.curators
    ~> jsonEmptyObject

decodeCurator :: Json -> Either String Curator
decodeCurator = decodeJson

decodeCurators :: Json -> Either String Curators
decodeCurators = decodeJson
