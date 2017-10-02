module Api.V3.MailGun where

import           Prelude (IO, ($), map, return)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Text (Text, intercalate, unpack)
import           Data.Text.Encoding (encodeUtf8)
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Conduit
import           Network.Mail.Mime (Address(..), Mail(..), renderMail')

addressToText :: Address -> Text
addressToText address =
    maybe "" (<> " ") (addressName address) <> "<" <> addressEmail address <> ">"

sendMailGun :: Text          -- ^ domain
            -> Text
            -> Manager       -- ^ re-use an existing http manager
            -> Mail
            -> IO ()
sendMailGun _ mailgunKey httpMan email = do
    bs <- renderMail' email
    request <- parseRequest $ unpack $ "https://api.mailgun.net/v3/dusk.host/messages.mime"
    preq <- postRequest (LBS.toStrict bs) request
    _ <- httpLbs (auth (encodeUtf8 mailgunKey) preq) httpMan
    return ()
  where
    to = encodeUtf8 $ intercalate "," $ map addressToText $ mailTo email
    auth = applyBasicAuth "api"
    postRequest message = formDataBody
      [ partBS "to" to
      , partFileBS "message" message
      ]

-- | similar to 'partFile', but useful if you have the file contents already in memory
-- Normally you would use partBS, but an API may require the parameter to be a file
partFileBS :: Text -> BS.ByteString -> Part
partFileBS n fileContents = partFileRequestBodyM n (unpack n) $
        return $ RequestBodyBS fileContents
