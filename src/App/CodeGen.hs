module App.CodeGen where

import Import hiding (Proxy)
import Data.Proxy
import Language.PureScript.Bridge
import Language.PureScript.Bridge.PSTypes (psInt)
import App.Form
import App.Crud

myTypes :: [SumType 'Haskell]
myTypes = [
    mkSumType (Proxy :: Proxy CuratorForm)
  , mkSumType (Proxy :: Proxy EventForm)
  , mkSumType (Proxy :: Proxy CreateResponse)
  ]

int64Bridge :: BridgePart
int64Bridge = typeName ^== "Int64" >> return psInt

main :: IO ()
main = writePSTypes "frontend/src/" (buildBridge mainBridge) myTypes
  where
    mainBridge = defaultBridge <|> int64Bridge

