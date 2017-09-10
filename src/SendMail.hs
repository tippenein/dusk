module SendMail
    ( sendMail

    -- | Re-exports
    , Address(..)
    , Mail(..)
    ) where

import           Import

import           Api.V3.MailGun
import           Control.Concurrent (forkIO)
import           Network.Mail.Mime (Address(..), Mail(..), renderMail')
import           System.Environment (getEnv)

sendMail :: Mail -> Handler ()
sendMail = send
  where
    send = if appSendMail compileTimeAppSettings
        then sendViaMailGun
        else sendToLog

sendViaMailGun :: Mail -> Handler ()
sendViaMailGun m = do
  manager <- appHttpManager <$> getYesod
  mailgunKey <- fmap (appMailgunKey . appSettings) getYesod
  void $ liftIO $ sendMailGun "dusk.host" mailgunKey manager m

sendToLog :: Mail -> Handler ()
sendToLog mail = do
    rendered <- liftIO $ renderMail' mail
    void $ $(logDebug) $ toStrict $ decodeUtf8 rendered
