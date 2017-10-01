module App.Form where

import Import

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
