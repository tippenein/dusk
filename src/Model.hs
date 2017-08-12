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
import Elm
import Elm.Export.Persist ()
import Elm.Export.Persist.BackendKey ()

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
    ident Text
    token UUID         sqltype=uuid
    invited_by UserId
    sent_at UTCTime

Event json sql=events
    name           Text
    description    Text Maybe
    asset_id       Text
    owner_id       UserId
    all_day        Bool          default=False
    start_datetime UTCTime
    end_datetime   UTCTime Maybe
    deriving Show Generic
|]

instance ElmType Event

deriving instance ElmType EventId

instance ElmType User

deriving instance ElmType UserId

type ControlIO m = (MonadIO m, MonadBaseControl IO m)

type DBM m a = (ControlIO m, MonadThrow m, Monad m) => SqlPersistT m a

type DB a = forall m. DBM m a

type DBVal val =
  ( PersistEntity val
  , PersistEntityBackend val ~ SqlBackend
  , PersistStore (PersistEntityBackend val))
