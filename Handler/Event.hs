module Handler.Event where

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
-- import Database.Persist
import Import


data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

getEventR :: Handler Html
getEventR = do
  (formWidget, formEnctype) <- generateFormPost sampleForm
  -- events <- runDB $ selectList [] []
  let submission = Nothing :: Maybe FileForm
  defaultLayout $ do
    setTitle "events!"
    $(widgetFile "events")

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
  <$> fileAFormReq "Choose a file"
  <*> areq textField textSettings Nothing
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

postEventR :: Handler Html
postEventR = do
  ((result, formWidget), formEnctype) <- runFormPost sampleForm
  let
    submission = case result of
      FormSuccess res -> Just res
      _ -> Nothing

  defaultLayout $ do
    setTitle "events!"
    $(widgetFile "events")
