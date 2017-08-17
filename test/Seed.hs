{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
import Import
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql (pgConnStr, withPostgresqlConn, runSqlConn)
import Data.Time.Clock

type EventTuple = (Text, Text, Text, UserId, Integer)

insertEvent :: MonadIO m => EventTuple-> ReaderT SqlBackend m ()
insertEvent (name, description, asset, uid, daysFromNow)= do
  (s,e) <- liftIO $ addDays daysFromNow
  insert_ $ Event name (Just description) asset uid False (Just s) (Just e)

insertUser :: MonadIO m => (Text, Role) -> ReaderT SqlBackend m ()
insertUser (ident, role) = do
  i <- insert $ User ident (Just ident)
  insert_ $ UserRole i role

users :: [(Text, Role)]
users = [
    ("Rufus Viken", Curator)
  , ("Gary Busey", Curator)
  , ("a fan", Fan)
  , ("tippenein@gmail.com", Admin)
  ]

events :: [EventTuple]
events =
  [ ("Zombie Shit Fest", mkDescript 2, "asdf", (toSqlKey 1), 11)
  , ("ROBOT PARTY",      mkDescript 1, "asdf", (toSqlKey 1), 12)
  , ("THE FEST 2017",    mkDescript 4, "asdf", (toSqlKey 2), 13)
  , ("Yerp Terp",        mkDescript 5, "asdf", (toSqlKey 1), 14)
  , ("Churrrrrrch",      mkDescript 3, "asdf", (toSqlKey 2), 15)
  ]
  where
    mkDescript n = intercalate "\n\n" $ lorems n

addDays :: Integer -> IO (UTCTime, UTCTime)
addDays n = do
  let days = nominalDays n
  th <- addUTCTime days <$> getCurrentTime
  return $ (,) th th

nominalDays :: Integer -> NominalDiffTime
nominalDays n = fromInteger $ 86400 * n

insertFixtures :: MonadIO m => ReaderT SqlBackend m ()
insertFixtures = do
  mapM_ insertUser users
  mapM_ insertEvent events

main :: IO ()
main = do
  settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
  let conn = (pgConnStr $ appDatabaseConf settings)
  runStderrLoggingT . withPostgresqlConn conn $ runSqlConn $ do
    runMigration migrateAll
    insertFixtures
