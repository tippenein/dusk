{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
import Import
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql (pgConnStr, withPostgresqlConn, runSqlConn)

insertUser :: MonadIO m => (Text, Role) -> ReaderT SqlBackend m ()
insertUser (ident, role) = do
  i <- insert $ User ident (Just ident)
  insert_ $ UserRole i role

users :: [(Text, Role)]
users =
  [ ("tippenein@gmail.com", Admin)
  , ("rufus", Curator)
  , ("a fan", Fan)
  ]

main :: IO ()
main = do
  settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
  let conn = (pgConnStr $ appDatabaseConf settings)
  runStderrLoggingT . withPostgresqlConn conn $ runSqlConn $ do
    runMigration migrateAll
    mapM_ insertUser users
