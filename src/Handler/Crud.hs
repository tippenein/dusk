module Handler.Crud where

import Database.Esqueleto
import Control.Lens.TH
import Data.Aeson.Types
import qualified Database.Persist as Persist
import Import
import Web.Internal.HttpApiData (parseQueryParamMaybe)

data PaginationParams
  = PaginationParams
  { _perPage :: Int
  , _page :: Int
  , _orderBy :: Maybe Text
  , _order :: Maybe Text
  }

list filters sorts params = selectList filters (withParams sorts)
  where
    pagination = getPaginationParams params
    withParams sorts =
      [OffsetBy (_page pagination), LimitTo (_perPage pagination)] ++ sorts

getPaginationParams ps = PaginationParams
  { _perPage = fromMaybe 50 $ getParam "per_page" ps
  , _page = fromMaybe 1 $ getParam "page" ps
  , _orderBy = getParam "order_by" ps
  , _order = getParam "order" ps
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
