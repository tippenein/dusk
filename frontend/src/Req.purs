module Req where
import Message as Msg
import Data.Argonaut.Generic.Aeson (encodeJson, decodeJson)
import Data.Either
import Data.Tuple
import App.Crud
import Helper

handleCreateResponse idx res =
  case decodeJson res.response of
    Left e -> Tuple Failure e
    Right cr -> case cr of
      CreateSuccess _ -> Tuple Success (Msg.t idx cr)
      FailedUniquenessConstraint -> Tuple Warning (Msg.t idx cr)
      CreateFailure t -> Tuple Failure (Msg.t idx cr)
