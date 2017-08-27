module Handler.Admin.Curator where

import qualified Data.UUID.V4 as UUID

import           Data.Aeson.Types
import           Helper.Validation (mkEmailAddress)
import qualified Text.Email.Validate as Email

import           Notification (sendInvite)
import           Import

respond201 :: Handler ()
respond201 = sendResponseStatus status201 ("CREATED" :: Text)

postAdminCuratorR :: Handler ()
postAdminCuratorR = do
  uid <- requireAuthId -- XXX check that this is same as posted value
  curatorPost <- requireJsonBody :: Handler CuratorForm
  now <- liftIO getCurrentTime
  tok <- liftIO UUID.nextRandom
  let invite = CuratorInvite (invitee curatorPost) uid tok now
  sendInvite invite
  respond201

data CuratorForm
  = CuratorForm
  { invitee :: Email.EmailAddress
  , inviter :: UserId
  } deriving Show

instance FromJSON CuratorForm where
  parseJSON (Object v) = do
    inviter <- v .: "inviter"
    email <- do
      e <- v .: "invitee"
      case mkEmailAddress e of
        Nothing -> fail "invalid email"
        Just m -> return m
    return (CuratorForm email inviter)
  parseJSON x = typeMismatch "CuratorInvite" x
