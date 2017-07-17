{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Model where

import ClassyPrelude.Yesod hiding (on, (==.), Value)
import Model.BCrypt
import Model.Instances
import Database.Esqueleto

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

-- Curator vs User ?
-- are Users who log into the site different than curators who throw the parties?

getUserPassword :: Text -> DB (Maybe (Entity User, Entity Password))
getUserPassword email = fmap listToMaybe $
  select $
  from $ \(user `InnerJoin` pass) -> do
  on (user ^. UserId ==. pass ^. PasswordUser)
  where_ (user ^. UserIdent ==. val email)
  return (user, pass)

getUserRoles :: UserId -> DB [Role]
getUserRoles uid = do
  v <- select $
    from $ \userRole -> do
    where_ (userRole ^. UserRoleUser_id ==. val uid)
    return $ userRole ^. UserRoleRole
  return $ map unValue v

getUsersWithRole :: Role -> DB ([Entity User])
getUsersWithRole role =
  select $
  from $ \(user `InnerJoin` userRole) -> do
  on (user ^. UserId ==. userRole ^. UserRoleUser_id)
  where_ (userRole ^. UserRoleRole ==. val role)
  return user

getUserEntity :: Text -> DB (Maybe (Entity User))
getUserEntity email = fmap listToMaybe $
  select $
  from $ \user -> do
  where_ (user ^. UserIdent ==. val email)
  return user

createUser :: Text -> Text -> DB (Entity User)
createUser email pass = do
  let newUser = User email Nothing
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
