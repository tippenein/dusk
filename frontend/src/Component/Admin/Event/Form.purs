module Component.Admin.Event.Form where

import Data.Lens
import Data.DateTime (DateTime)
import FormValidation (FormValue')

type EventForm =
  { name :: FormValue' String
  , description :: FormValue' String
  , startDatetime :: FormValue' String
  , endDatetime :: FormValue' String
  }
_name :: Lens' EventForm (FormValue' String)
_name = lens _.name _ { name = _ }

_description :: Lens' EventForm (FormValue' String)
_description = lens _.description _ { description = _ }

_startDatetime :: Lens' EventForm (FormValue' String)
_startDatetime = lens _.startDatetime _ { startDatetime = _ }

_endDatetime :: Lens' EventForm (FormValue' String)
_endDatetime = lens _.endDatetime _ { endDatetime = _ }
