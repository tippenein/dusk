module Handler.Curator where

import Import
import Model.User

getCuratorsR :: Handler Value
getCuratorsR = do
  curators <- runDB $ getUsersWithRole Curator
  return $ object ["curators" .= curators]
