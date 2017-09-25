{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
import Import
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql (pgConnStr, withPostgresqlConn, runSqlConn)
import Data.Time.Clock
import Prelude ((!!))
import System.Random (randomRIO)

type EventTuple = (Text, Text, UserId, Integer)

pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

images =
  [ Just "logo/upload-aBjgXjqT"
  , Just "logo/upload-RU0ZjtRf"
  , Just "logo/upload-Dn0y-DN_"
  , Nothing
  ]

insertEvent :: MonadIO m => EventTuple-> ReaderT SqlBackend m ()
insertEvent (name, description, uid, daysFromNow)= do
  (s,e) <- liftIO $ addDays daysFromNow
  i <- liftIO $ pick images
  insert_ $ Event name (Just description) i uid False (Just s) (Just e)

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
  [ ("Zombie Shit Fest", mkDescript 2, (toSqlKey 1), 11)
  , ("ROBOT PARTY",      mkDescript 1, (toSqlKey 1), 12)
  , ("THE FEST 2017",    mkDescript 4, (toSqlKey 2), 13)
  , ("Yerp Terp",        mkDescript 5, (toSqlKey 1), 14)
  , ("Churrrrrrch",      mkDescript 3, (toSqlKey 2), 15)
  , ("Bogus Bonanza",    mkDescript 5, (toSqlKey 1), 20)
  , ("FIEF FEST",        mkDescript 3, (toSqlKey 2), 21)
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
