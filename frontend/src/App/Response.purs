module App.Response where

import Import
import Data.Lens (_Just, (^.), (^?))
import App.CodeGen (_CreateFailure, _CreateSuccess)
import Helper (Message(..))
import Data.Argonaut.Generic.Aeson (decodeJson)

-- handleCreateResponse :: _ -> Tuple Message String
handleCreateResponse res = do
  case decodeJson res.response of
    Left e -> Tuple Failure e
    Right cr -> case (cr ^? _CreateSuccess) of
          Just f -> Tuple Success $ show f
          Nothing -> Tuple Warning (cr ^? _CreateFailure ^. _Just)
