module App.Data.Curator where

import Import

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Text.Email.Parser (toString)
import Text.Email.Validate (EmailAddress)

type CuratorId = Int

newtype CuratorInvite = CuratorInvite
  { invitee :: EmailAddress
  , inviter :: CuratorId
  }

instance encodeCuratorInvite :: EncodeJson CuratorInvite where
  encodeJson (CuratorInvite curatorInvite)
     = "invitee" := toString curatorInvite.invitee
    ~> "inviter" := curatorInvite.inviter
    ~> jsonEmptyObject

newtype Curator = Curator
  { id :: Int
  , name :: String
  , ident :: String
  }

instance decodeJsonCurator :: DecodeJson Curator where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    name <- obj .? "name"
    ident <- obj .? "ident"
    pure $ Curator {
        id
      , name
      , ident
      }

instance encodeCurator :: EncodeJson Curator where
  encodeJson (Curator curator)
     = "id" := curator.id
    ~> "name" := curator.name
    ~> "ident" := curator.ident
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
