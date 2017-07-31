module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  events <- runDB $ selectList [] [Desc EventStart_datetime]
  defaultLayout $ do
    setTitle "RSVP"
    $(widgetFile "homepage")
