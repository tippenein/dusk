module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  events <- runDB $ selectList [] [Asc EventStart_datetime]
  defaultLayout $ do
    setTitle "Dusk"
    $(widgetFile "homepage")
