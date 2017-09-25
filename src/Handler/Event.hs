{-# LANGUAGE OverloadedLists #-}
module Handler.Event where


import Import

import qualified Handler.Crud as Crud
import qualified Data.Map as Map

getEventR :: EventId -> Handler Value
getEventR event_id = do
  event <- runDBor404 $ get event_id
  return $ object ["event" .= (Entity event_id event)]


getEventsR :: Handler Value
getEventsR = do
  now <- liftIO getCurrentTime
  params <- Map.fromList <$> (reqGetParams <$> getRequest)
  events <- runDB $ do
    Crud.list [ EventStart_datetime >=. Just now ] [Asc EventStart_datetime] params
  return $ object ["events" .= events]
