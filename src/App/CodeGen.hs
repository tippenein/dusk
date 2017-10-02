module App.CodeGen where

import Import hiding (Proxy)
import Data.Proxy
import Language.PureScript.Bridge
import Language.PureScript.Bridge.PSTypes (psInt)
import App.Form
import App.Crud
import Control.Lens

psDate :: TypeInfo 'PureScript
psDate = TypeInfo {
    _typePackage = ""
  , _typeModule = "Types"
  , _typeName = "Date"
  , _typeParameters = []
  }

-- | Use type definition in frontend's Types.purs
psClientType :: MonadReader BridgeData m => m PSType
psClientType = do
  inType <- view haskType
  params <- psTypeParameters
  return TypeInfo {
    _typePackage = ""
  , _typeModule  = "Types"
  , _typeName = inType ^. typeName
  , _typeParameters = params
  }

myTypes :: [SumType 'Haskell]
myTypes = [
    mkSumType (Proxy :: Proxy CuratorForm)
  , mkSumType (Proxy :: Proxy EventForm)
  , mkSumType (Proxy :: Proxy CreateResponse)
  , mkSumType (Proxy :: Proxy PaginationParams)
  , mkSumType (Proxy :: Proxy FilterParams)
  -- , mkSumType (Proxy :: Proxy Event)
  -- , mkSumType (Proxy :: Proxy User)
  , mkSumType (Proxy :: Proxy FExp)
  ]

-- There is currently no Generic instance for PureScript's Data.Date,
-- so just use the JSON string in the frontend:
utcTimeBridge :: BridgePart
utcTimeBridge = typeName ^== "UTCTime" >> return psDate

int64Bridge :: BridgePart
int64Bridge = typeName ^== "Int64" >> return psInt

mainBridge :: BridgePart
mainBridge = defaultBridge
  <|> (typeName ^== "Key" >> psClientType)
  <|> utcTimeBridge
  <|> int64Bridge

main :: IO ()
main = writePSTypes "frontend/src/" (buildBridge mainBridge) myTypes
