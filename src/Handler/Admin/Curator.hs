module Handler.Admin.Curator where

import qualified Data.UUID.V4 as UUID

import           Import
import Data.Aeson.Types
import qualified Text.Email.Validate as Email
import Helper.Validation (mkEmailAddress)

respond201 :: Handler ()
respond201 = sendResponseStatus status201 ("CREATED" :: Text)

postAdminCuratorR :: Handler ()
postAdminCuratorR = do
  uid <- requireAuthId
  curatorPost <- requireJsonBody :: Handler CuratorForm
  now <- liftIO getCurrentTime
  tok <- liftIO UUID.nextRandom
  let invite = CuratorInvite (invitee curatorPost) tok uid now
  _ <- runDB $ insert invite
  respond201

data CuratorForm
  = CuratorForm
  { invitee :: Email.EmailAddress }
  deriving Show

instance FromJSON CuratorForm where
  parseJSON (Object v) = do
    email <- do
      e <- v .: "email"
      case mkEmailAddress e of
        Nothing -> fail "invalid email"
        Just m -> return m
    return (CuratorForm email)
  parseJSON x = typeMismatch "CuratorInvite" x
