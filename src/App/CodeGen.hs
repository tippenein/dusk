module App.CodeGen where

import Import hiding (Proxy)
import Data.Aeson.Types
import Data.Proxy
import qualified Text.Email.Validate as Email
import Language.PureScript.Bridge
import Language.PureScript.Bridge.PSTypes (psInt)
import Model.Instances()
import Handler.Crud

data CuratorForm
  = CuratorForm
  { cf_invitee :: Text
  } deriving (Generic, Typeable, Show)

instance FromJSON CuratorForm

data EventForm
  = EventForm
  { ef_name :: Text
  , ef_description :: Maybe Text
  , ef_startDatetime :: Maybe Text
  , ef_endDatetime :: Maybe Text
  , ef_logo :: Maybe Text
  } deriving (Generic, Typeable, Show)

instance FromJSON EventForm

myTypes :: [SumType 'Haskell]
myTypes = [
    mkSumType (Proxy :: Proxy CuratorForm)
  , mkSumType (Proxy :: Proxy EventForm)
  , mkSumType (Proxy :: Proxy CreateResponse)
  ]

int64Bridge :: BridgePart
int64Bridge = typeName ^== "Int64" >> return psInt

mainBridge = defaultBridge <|> int64Bridge

main :: IO ()
main = writePSTypes "frontend/src/" (buildBridge mainBridge) myTypes
