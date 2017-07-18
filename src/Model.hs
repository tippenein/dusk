{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Model where

import ClassyPrelude.Yesod hiding (on, (==.), Value)
import Database.Esqueleto
import Model.BCrypt
import Model.Instances

type Validated a = Either [Text] a
type Validation a = a -> Validated a

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
    ident Text
    name Text Maybe
    UniqueUser ident
    deriving Show

UserRole sql=user_roles
    user_id UserId
    role Role
    UniqueUserRole user_id role
    deriving Show

Password sql=passwords
    hash BCrypt
    user UserId
    UniquePasswordUser user
    deriving Show

PasswordReset sql=password_resets
    token Text
    created UTCTime
    user UserId

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

Event sql=events
    name Text
    description Text Maybe
    asset_id Text
    owner_id UserId
    start_day Day
    end_day Day Maybe
    start_time UTCTime Maybe
    end_time UTCTime Maybe
|]



type ControlIO m = (MonadIO m, MonadBaseControl IO m)

type DBM m a = (ControlIO m, MonadThrow m, Monad m) => SqlPersistT m a

type DB a = forall m. DBM m a

type DBVal val =
  ( PersistEntity val
  , PersistEntityBackend val ~ SqlBackend
  , PersistStore (PersistEntityBackend val))
