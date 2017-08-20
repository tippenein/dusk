module Component.NotFound (view) where

import Import
import Halogen.HTML hiding (map)

view s = h1_ [ text s]
