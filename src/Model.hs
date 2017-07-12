{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Model where

import ClassyPrelude.Yesod hiding (on, (==.))
import Model.BCrypt
-- import Model.Instances
import Database.Esqueleto
import Database.Persist.Sql ()

-- import qualified Data.ByteString.Char8 as B8
-- import qualified Data.UUID as UUID


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
    ident Text
    UniqueUser ident
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

Rsvp sql=rsvps
    email Text Maybe
    phone Text Maybe
    name Text

Event sql=events
    name Text
    description Text Maybe
    asset_id Text
    day_only Bool default=False
    start_time UTCTime Maybe default=now()
    end_time UTCTime Maybe
|]

-- Curator vs User ?
-- are Users who log into the site different than curators who throw the parties?

getUserPassword :: Text -> DB (Maybe (Entity User, Entity Password))
getUserPassword email = fmap listToMaybe $
  select $
  from $ \(user `InnerJoin` pass) -> do
  on (user ^. UserId ==. pass ^. PasswordUser)
  where_ (user ^. UserIdent ==. val email)
  return (user, pass)

getUserEntity :: Text -> DB (Maybe (Entity User))
getUserEntity email = fmap listToMaybe $
  select $
  from $ \user -> do
  where_ (user ^. UserIdent ==. val email)
  return user

createUser :: Text -> Text -> DB (Entity User)
createUser email pass = do
  let newUser = User email
  userId <- insert $ newUser
  h <- liftIO $ hashPassword pass
  _ <- insert $ Password h userId
  return (Entity userId newUser)


type ControlIO m = (MonadIO m, MonadBaseControl IO m)

type DBM m a = (ControlIO m, MonadThrow m, Monad m) => SqlPersistT m a

type DB a = forall m. DBM m a

type DBVal val =
  ( PersistEntity val
  , PersistEntityBackend val ~ SqlBackend
  , PersistStore (PersistEntityBackend val))
