module Handler.Profile where

import Import

getProfileR :: Handler Value
getProfileR = do
  (uid, user) <- requireAuthPair
  return $ object [ "user_ident" .= userIdent user
                  , "user_name"  .= userName user
                  , "user_id"    .= uid ]
