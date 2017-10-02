module App.Crud where

import Database.Esqueleto
import Control.Lens.TH
import Data.Aeson.Types
import qualified Database.Persist as Persist
import Import
import Web.Internal.HttpApiData (parseQueryParamMaybe)

data FilterParams
  = FilterParams
  { f_expressions :: [FExp]
  } deriving (Generic, Typeable, Show)

data FExp
  = IsEq FExp FExp
  | IsNotEq FExp FExp
  | IsEmpty FExp
  | IsNotEmpty FExp
  | LessThan FExp FExp
  | GreaterThan FExp FExp
  | FParam Text
  | FValue Text
  deriving (Generic, Typeable, Show)

data PaginationParams
  = PaginationParams
  { pp_perPage :: Int
  , pp_page :: Int
  , pp_orderBy :: Maybe Text
  , pp_order :: Maybe Text
  } deriving (Generic, Typeable, Show)

list filters sorts_ params = selectList filters (withParams sorts_)
  where
    pagination = getPaginationParams params
    withParams sorts =
      [OffsetBy (_page pagination), LimitTo (_perPage pagination)] ++ sorts

getPaginationParams ps = PaginationParams
  { pp_perPage = fromMaybe 50 $ getParam "per_page" ps
  , pp_page = fromMaybe 1 $ getParam "page" ps
  , pp_orderBy = getParam "order_by" ps
  , pp_order = getParam "order" ps
  }
  where
    getParam p qs = case lookup p qs of
      Just "" -> Nothing
      Nothing -> Nothing
      Just v -> parseQueryParamMaybe v

-- | use insertKey if you have uuid keys
create e = do
  k <- Persist.insert e
  return $ CreateSuccess $ fromSqlKey k

createUnique e = do
  k <- Persist.insertUnique e
  case k of
    Nothing -> return FailedUniquenessConstraint
    Just k' -> return $ CreateSuccess $ fromSqlKey k'

data CreateResponse
  = CreateSuccess Int64
  | CreateFailure Text
  | FailedUniquenessConstraint
  deriving (Generic, Typeable, Show)

makePrisms ''CreateResponse

instance ToJSON CreateResponse where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON CreateResponse
