module App.Data.Profile where

import Import

import Data.Argonaut (class DecodeJson, Json, decodeJson, (.?))

newtype Profile = Profile
  { user_id :: Int
  , user_ident :: String
  , user_name :: Maybe String
  }

derive instance newtypeProfile :: Newtype Profile _

instance decodeJsonProfile :: DecodeJson Profile where
  decodeJson json = do
    obj <- decodeJson json
    user_id <- obj .? "user_id"
    user_ident <- obj .? "user_ident"
    user_name <- obj .? "user_name"
    pure $ Profile {
        user_id
      , user_ident
      , user_name
      }

decodeProfile :: Json -> Either String Profile
decodeProfile = decodeJson
