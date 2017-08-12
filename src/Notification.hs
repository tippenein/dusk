module Notification where

import Import
import Model.User
import SendMail

import Data.Text.Lazy.Builder (toLazyText)
import Network.Mail.Mime (simpleMail')
import Text.Shakespeare.Text (textFile)

data Notification = CuratorInvite CuratorInvite

type Token = UUID

-- sendNotification :: Notification -> Handler ()
-- sendNotification n = do
--     recipients <- runDB $ notificationRecipients n

--     mapM_ (sendMail <=< notificationToMail n) recipients

-- notificationToMail :: Notification -> CuratorInvite -> Handler Mail
-- notificationToMail n r = do
--     let c = notificationComment n
--         subject = "You've been invited to Curate"
--         comment = unMarkdown $ commentBody c
--         baseUrl = siteBaseUrl $ notificationSite n
--         articleUrl = commentArticleURL c

--     body <- toLazyText <$> withUrlRenderer $(textFile "templates/mail/new_curator.text")

--     return $ simpleMail' (curatorInviteIdent r) (curatorInviteInvitedBy n) subject body

-- notificationRecipients :: Notification -> DB [Recipient]
-- notificationRecipients n = do
--     let c = notificationComment n

--     subs <- activeSubscriptions' $ notificationName n
--     users <- findUsers $ filter (/= commentUser c) $ map subscriptionUser subs

--     fmap catMaybes $ forM users $ \(Entity uid u) -> do
--         let msub = find ((== uid) . subscriptionUser) subs

--         return $ fmap (Recipient u . subscriptionToken) msub
