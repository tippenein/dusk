module Handler.Admin.Curator where

import           Yesod.Form.Bootstrap3 (BootstrapFormLayout(..), renderBootstrap3)
import qualified Data.UUID.V4 as UUID

import           Import

postAdminCuratorR :: Handler Html
postAdminCuratorR = do
  ((result, _formWidget), _formEncType) <- runFormPost curatorForm
  userId  <- requireAuthId
  case result of
    FormSuccess (CuratorForm email) -> do
      invite <- liftIO $ curatorFormToInvite email userId
      case invite of
        Left msg -> do
          setMessage $ toHtml msg
          redirect $ AdminCuratorR
        Right c -> do
          _ <- runDB $ insert c
          setMessage "Curator Invite Sent"
          redirect $ AdminCuratorR

    FormFailure reasons -> defaultLayout $ do
      setMessage $ toHtml $ unlines reasons
      redirect $ AdminCuratorR
    _ -> defaultLayout $ do
      setMessage "something went wrong"
      redirect $ AdminCuratorR

curatorFormToInvite :: Text -> UserId -> IO (Either String CuratorInvite)
curatorFormToInvite email uid = do
  now <- getCurrentTime
  tok <- UUID.nextRandom
  return $ Right $ CuratorInvite email tok uid now

data CuratorForm = CuratorForm Text

curatorForm :: Form CuratorForm
curatorForm = renderBootstrap3 BootstrapBasicForm $
  CuratorForm <$> areq textField (named "email" "email") Nothing
