module Component.Admin.Event.Form where

import Data.Lens

type EventForm =
  { name :: String
  , description :: String
  , startDatetime :: String
  , endDatetime :: String
  }

_name :: Lens' EventForm String
_name = lens _.name _ { name = _ }

_description :: Lens' EventForm String
_description = lens _.description _ { description = _ }

_startDatetime :: Lens' EventForm String
_startDatetime = lens _.startDatetime _ { startDatetime = _ }

_endDatetime :: Lens' EventForm String
_endDatetime = lens _.endDatetime _ { endDatetime = _ }
