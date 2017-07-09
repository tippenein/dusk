{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Data.UUID (UUID)
import qualified Data.ByteString.Char8 as B8
import qualified Data.UUID as UUID
import Database.Persist.Sql

instance PersistField UUID where
  toPersistValue u = PersistDbSpecific . B8.pack . UUID.toString $ u
  fromPersistValue (PersistDbSpecific t) =
    case UUID.fromString $ B8.unpack t of
      Just x -> Right x
      Nothing -> Left "Invalid UUID"
  fromPersistValue _ = Left "Not PersistDBSpecific"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
    ident Text
    password Text Maybe
    UniqueUser ident
    deriving Typeable

Email sql=emails
    email Text
    userId UserId Maybe
    verkey Text Maybe
    UniqueEmail email

Rsvp sql=rsvps
    email Text Maybe
    phone Text Maybe
    name Text

Event sql=events
    name Text
    description Text Maybe
    asset_id Text
|]
