{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.Instances
  ( Role(..)
  , UUID
  ) where

import           ClassyPrelude.Yesod

import qualified Data.ByteString.Char8 as B8
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import           Database.Persist.Sql

instance ToJSON UUID where
  toJSON = String . pack . UUID.toString

instance FromJSON UUID where
  parseJSON (String s) = case UUID.fromString $ unpack s of
    Nothing -> error "invalid UUID"
    Just a -> return a
  parseJSON _ = error "invalid UUID"

instance PersistField UUID where
  toPersistValue u = PersistDbSpecific . B8.pack . UUID.toString $ u
  fromPersistValue (PersistDbSpecific t) =
    case UUID.fromString $ B8.unpack t of
      Just x -> Right x
      Nothing -> Left "Invalid UUID"
  fromPersistValue _ = Left "Not PersistDBSpecific"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

data Role = Admin | Curator | Fan
  deriving (Show, Read, Eq)

derivePersistField "Role"
