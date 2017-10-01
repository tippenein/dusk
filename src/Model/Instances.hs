{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.Instances
  ( Role(..)
  , UUID
  ) where

import           ClassyPrelude.Yesod

import qualified Data.Text.Encoding as Encoding
import qualified Data.ByteString.Char8 as B8
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import           Database.Persist.Sql
import qualified Text.Email.Validate as Email
import           Text.Email.Validate (EmailAddress)

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

validEmail t =
    case (Email.emailAddress =<< Email.canonicalizeEmail t) of
      Just x -> Right x
      Nothing -> Left "Invalid Email"

instance FromJSON EmailAddress where
  parseJSON (String e) = case Email.validate (Encoding.encodeUtf8 e) of
    Left _ -> error "invalid email"
    Right r -> pure r
  parseJSON _ = fail "not email"

instance ToJSON EmailAddress where
  toJSON = String . pack . B8.unpack . Email.toByteString

derivePersistField "EmailAddress"
