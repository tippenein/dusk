module Handler.Event where

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
-- import Database.Persist
import Import


-- data EventForm = EventForm
--     { _logo_image :: FileInfo
--     , _name :: Text
--     , _event_description :: Text
--     }

getEventShowR :: Key Event -> Handler Html
getEventShowR k = undefined  --do
  -- events <- runDB $ get k

getEventR :: Handler Html
getEventR = do
  events <- runDB $ selectList [] [Desc EventName]
  (formWidget, formEnctype) <- generateFormPost eventForm
  let submission = Nothing :: Maybe Event
  defaultLayout $ do
    setTitle "events!"
    $(widgetFile "events")

postEventR :: Handler Html
postEventR = do
  ((result, formWidget), formEnctype) <- runFormPost eventForm
  events <- runDB $ selectList [] [Desc EventName]
  case result of
    FormSuccess res -> do
      entryId <- runDB $ insert res
      redirect $ EventShowR entryId
    _ -> defaultLayout $ do
            setTitle "events!"
            $(widgetFile "events")


eventForm :: Form Event
eventForm = renderBootstrap3 BootstrapBasicForm $
  Event
    <$> areq textField (textSettings { fsLabel = "event name"}) Nothing
    <*> aopt textField textSettings Nothing
    <*> pure Nothing -- (fileContent <$> fileAFormOpt "Choose a file")
  -- Add attributes like the placeholder and CSS classes.
  where textSettings = FieldSettings
          { fsLabel = "event description"
          , fsTooltip = Nothing
          , fsId = Nothing
          , fsName = Nothing
          , fsAttrs =
              [ ("class", "form-control")
              , ("placeholder", "Event description")
              ]
          }
