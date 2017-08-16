module App.View.NotFound where

import Prelude ((<>), ($))
-----
import App.Route (Location, pathFromRoute)
-----
import Halogen as H
import Halogen.HTML as HH

data Query a
  = SetUsername String a
  | MakeRequest a
render :: Location -> H.ComponentHTML Query
render r = HH.text $ "Not found: " <> pathFromRoute r
