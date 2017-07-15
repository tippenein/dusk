module Handler.Profile where

import Import

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s User page"
        [whamlet|
<div .ui.container>

    <h1>
      Hello <strong><span class="username">#{userIdent user}</span></strong>!
|]
