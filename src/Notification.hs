module Notification where

import Import
import SendMail

import Data.Text.Lazy.Builder (toLazyText)
import Network.Mail.Mime (simpleMail')
import Text.Shakespeare.Text (textFile)
import qualified Text.Email.Validate as Email

type Token = UUID

sendInvite :: CuratorInvite -> Handler ()
sendInvite i =
  sendMail =<< inviteToMail i

inviteToMail :: CuratorInvite -> Handler Mail
inviteToMail (CuratorInvite invitee inviter _ _) = do
  site <- appSettings <$> getYesod
  user <- runDBor404 $ get inviter

  let subject = "You've been invited by " <> person <> " to join " <> siteName
      siteName = fromMaybe "http://dusk.host" $ appRoot site
      person :: Text = fromMaybe "an anonymous curator" $ userName user
      from = Address (Just "dusk.host") siteName

  body <- toLazyText <$> withUrlRenderer $(textFile "templates/mail/new_curator.text")

  return $ simpleMail' (simpleAddress invitee) from subject body

simpleAddress :: Email.EmailAddress -> Address
simpleAddress e = Address Nothing (decodeUtf8 $ Email.toByteString e)
