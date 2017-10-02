-- | Server specified encodings
module Types where

import Prelude

import Data.Generic (class Generic)
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Helper.Format
import Data.Maybe

data DateStamp = DateStamp DateTime

dateToString :: DateStamp -> Maybe String
dateToString (DateStamp dt) = formatDateTime dt

derive instance genericDateStamp :: Generic DateStamp

newtype Key a = Key Int

derive instance genericKey :: Generic (Key a)

instance eqKey :: Eq (Key a) where
  eq (Key a) (Key b) = eq a b

instance ordKey :: Ord (Key a) where
  compare (Key a) (Key b) = compare a b
