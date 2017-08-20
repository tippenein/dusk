module App.State where

import Prelude
import App.Router (Location(..))

type State
  = { appRoot :: String
    , location :: Location
    }

init :: State
init =
  { appRoot: ""
  , location: HomeR
  }
