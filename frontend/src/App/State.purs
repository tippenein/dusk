module App.State where

import Prelude
import App.Route (Location(..))

type State
  = { appRoot :: String
    , location :: Location
    }
init :: State
init =
  { appRoot: ""
  , location: Home
  }
