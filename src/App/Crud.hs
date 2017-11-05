module App.Crud where

import Database.Esqueleto
import Control.Lens.TH
import Data.Aeson.Types
import qualified Database.Persist as Persist
import Import
import Web.Internal.HttpApiData (parseQueryParamMaybe)

data FilterParams
  = FilterParams
  { f_expressions :: [FilterExpr]
  } deriving (Generic, Typeable, Show)

data FilterExpr
  = IsEq FilterExpr FilterExpr
  | IsNotEq FilterExpr FilterExpr
  | IsEmpty FilterExpr
  | IsNotEmpty FilterExpr
  | LessThan FilterExpr FilterExpr
  | GreaterThan FilterExpr FilterExpr
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

findBy i = do
  e <- runDBor404 $ get i
  return $ Entity i e

list filters sorts_ params = selectList filters (withParams sorts_)
  where
    pagination = getPaginationParams params
    withParams sorts =
      [OffsetBy (pp_page pagination - 1), LimitTo (pp_perPage pagination)] ++ sorts

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

delete k =
  runDB $ Persist.delete k

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

data GetResponse e
  = PaginatedResponse PaginationParams [e]
  | NonPaginatedResponse [e]
  | FindResponse e
  | NotFound Text
  deriving (Generic, Typeable, Show)

-- data PaginatedResponse e = PaginatedResponse PaginationParams e
--   deriving (Generic, Typeable, Show)
-- instance (ToJSON a) => ToJSON (PaginatedResponse a) where
--   toEncoding = genericToEncoding defaultOptions
-- instance (FromJSON a) => FromJSON (PaginatedResponse a)
-- data NonPaginatedResponse e = NonPaginatedResponse e
--   deriving (Generic, Typeable, Show)

-- instance (ToJSON a) => ToJSON (NonPaginatedResponse a) where
--   toEncoding = genericToEncoding defaultOptions
-- instance (FromJSON a) => FromJSON (NonPaginatedResponse a)

instance FromJSON PaginationParams
instance ToJSON PaginationParams where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON CreateResponse where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON CreateResponse

instance (ToJSON a) => ToJSON (GetResponse a) where
  toEncoding = genericToEncoding defaultOptions
instance (FromJSON a) => FromJSON (GetResponse a)

instance FromJSON FilterExpr
makePrisms ''CreateResponse
makePrisms ''GetResponse
makePrisms ''FilterExpr
