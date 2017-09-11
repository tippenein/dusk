module App.CodeGen where

import Import hiding (Proxy)
import Data.Aeson.Types
import Data.Proxy
import qualified Text.Email.Validate as Email
import Language.PureScript.Bridge
import Language.PureScript.Bridge.PSTypes (psInt)
import Model.Instances()

data CuratorForm
  = CuratorForm
  { cf_invitee :: Text
  } deriving (Generic, Typeable, Show)

instance FromJSON CuratorForm

data CreateResponse
  = CreateSuccess Int64
  | CreateFailure Text
  deriving (Generic, Typeable, Show)

instance ToJSON CreateResponse
instance FromJSON CreateResponse

data EventForm
  = EventForm
  { ef_name :: Text
  , ef_description :: Maybe Text
  , ef_startDatetime :: Maybe Text
  , ef_endDatetime :: Maybe Text
  , ef_logo :: Maybe Text
  } deriving (Generic, Typeable, Show)

instance FromJSON EventForm

data EventCreateResponse = EventCreateResponse { ecr_id :: Either Text Int64 }
  deriving (Generic, Typeable, Show)

instance ToJSON EventCreateResponse where
  toEncoding = genericToEncoding defaultOptions

myTypes :: [SumType 'Haskell]
myTypes = [
    mkSumType (Proxy :: Proxy CuratorForm)
  , mkSumType (Proxy :: Proxy CreateResponse)
  , mkSumType (Proxy :: Proxy EventForm)
  , mkSumType (Proxy :: Proxy EventCreateResponse)
  ]

int64Bridge :: BridgePart
int64Bridge = typeName ^== "Int64" >> return psInt

mainBridge = defaultBridge <|> int64Bridge

main :: IO ()
main = writePSTypes "frontend/src/" (buildBridge mainBridge) myTypes
