module Handler.Curator where

import Import

getCuratorR :: UserId -> Handler Html
getCuratorR i = do
  Just u <- runDB $ get i
  defaultLayout $ do
    setTitle $ toHtml $ uname u
    [whamlet|
<div .ui.container>
    <h1>#{uname u}
|]

uname :: User -> Text
uname = fromMaybe "Anonymous" . userName

getCuratorsR :: Handler Html
getCuratorsR = do
  curators <- runDB $ getUsersWithRole Curator
  defaultLayout $ do
    setTitle "curators"
    [whamlet|
<div .ui.container>
    <h1>Curators
    $forall Entity curatorId curator <- curators
        <li><a href=@{CuratorR curatorId}>#{uname curator}
|]
