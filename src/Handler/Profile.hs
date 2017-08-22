module Handler.Profile where

import Import

getProfileR :: Handler Value
getProfileR = do
  (_, user) <- requireAuthPair
  return $ object [ "user" .= user]
