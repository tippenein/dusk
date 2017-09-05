module Handler.Event where


import Import


getEventR :: EventId -> Handler Value
getEventR event_id = do
  event <- runDBor404 $ get event_id
  return $ object ["event" .= (Entity event_id event)]


getEventsR :: Handler Value
getEventsR = do
  now <- liftIO getCurrentTime
  events <- runDB $ selectList [ EventStart_datetime >=. Just now ] [Asc EventStart_datetime]
  return $ object ["events" .= events]
