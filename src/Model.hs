{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Model where

import ClassyPrelude.Yesod hiding (on, (==.), Value)
import Database.Esqueleto
import Model.Instances
import Helper.Util (presign)
import Text.Email.Validate

type Validated a = Either [Text] a
type Validation a = a -> Validated a

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json sql=users
    ident Text
    name Text Maybe
    UniqueUser ident
    deriving Show Generic

UserRole sql=user_roles
    user_id UserId
    role Role
    UniqueUserRole user_id role
    deriving Show

Email sql=emails
    email Text
    userId UserId Maybe
    verkey Text Maybe
    UniqueEmail email

Rsvper sql=fans
    email Text Maybe
    phone Text Maybe
    name Text

Rsvp sql=rsvps
    fan_id RsvperId
    event_id EventId

CuratorInvite sql=curator_invites
    email EmailAddress
    invited_by UserId
    token UUID         sqltype=uuid
    UniqueRecipient email
    sent_at UTCTime

Event sql=events
    name           Text
    description    Text Maybe
    asset_id       Text Maybe
    owner_id       UserId
    all_day        Bool          default=False
    start_datetime UTCTime Maybe
    end_datetime   UTCTime Maybe
    deriving Show Generic
|]

formatDateTime :: UTCTime -> Text
formatDateTime = pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

instance ToJSON (Entity Event) where
  toJSON (Entity i Event{..}) = object [
      "id" .= i
    , "name" .= eventName
    , "description" .= eventDescription
    , "asset_id" .= if isJust eventAsset_id then (presign <$> eventAsset_id) else Nothing
    , "owner_id" .= toJSON eventOwner_id
    , "all_day" .= toJSON eventAll_day
    , "start_datetime" .= (formatDateTime <$> eventStart_datetime)
    , "end_datetime" .= (formatDateTime <$> eventStart_datetime)
    ]

type ControlIO m = (MonadIO m, MonadBaseControl IO m)

type DBM m a = (ControlIO m, MonadThrow m, Monad m) => SqlPersistT m a

type DB a = forall m. DBM m a

type DBVal val =
  ( PersistEntity val
  , PersistEntityBackend val ~ SqlBackend
  , PersistStore (PersistEntityBackend val))
