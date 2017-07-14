module Handler.Event where

import Data.Time.Clock as Clock

import qualified Data.Text as T

import Import

getEventR :: Handler Html
getEventR = do
  events <- runDB $ selectList [] [Desc EventStart_time]
  defaultLayout $ do
    setTitle "events!"
    $(widgetFile "events")

