module Helper.Validation where

import Import.NoFoundation
import qualified Data.ByteString.Char8 as B8
import qualified Text.Email.Validate as Email


mkEmailAddress :: String -> Maybe Email.EmailAddress
mkEmailAddress = Email.emailAddress . B8.pack
