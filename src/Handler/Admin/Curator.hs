module Handler.Admin.Curator where

import           App.Crud
import           App.Form
import           Control.Lens
import qualified Data.UUID.V4 as UUID
import           Helper.Validation (mkEmailAddress)
import           Import
import           Notification (sendInvite)

respond201 :: Handler ()
respond201 = sendResponseStatus status201 ("CREATED" :: Text)

postAdminCuratorR :: Handler Value
postAdminCuratorR = do
  uid <- requireAuthId -- XXX check that this is same as posted value?
  curatorPost <- requireJsonBody :: Handler CuratorForm
  now <- liftIO getCurrentTime
  tok <- liftIO UUID.nextRandom
  let e = mkEmailAddress $ unpack (cf_invitee curatorPost)
  case e of
    Nothing -> do
      $(logWarn) "did not send invalid email"
      return $ toJSON $ CreateFailure "invalid email address"
    Just e' -> do
      let invite = CuratorInvite e' uid tok now
      entry <- runDB $ createUnique invite
      if (isJust $ entry ^? _CreateSuccess ^? _Just)
        then sendInvite invite
        else $(logInfo)  ("did not send invite to " <> tshow e')
      return $ toJSON entry
