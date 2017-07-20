module Handler.Event where


import Import


getEventR :: EventId -> Handler Html
getEventR event_id = do
  event <- runDBor404 $ get event_id
  defaultLayout $ do
    setTitle' $ eventName event
    $(widgetFile "event")

getEventsR :: Handler Html
getEventsR = do
  events <- runDB $ selectList [] [Desc EventStart_datetime]
  defaultLayout $ do
    setTitle "events!"
    $(widgetFile "events")

