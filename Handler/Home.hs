module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  let handlerName = "getHomeR" :: Text
  defaultLayout $ do
    aDomId <- newIdent
    setTitle "Welcome To Yesod!"
    $(widgetFile "homepage")
