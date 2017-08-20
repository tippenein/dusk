module Helper (
    styleClass
  , apiUrl
  , placeholder
  ) where

import Halogen.HTML.Properties as HP
import Halogen.HTML (img, ClassName(..))
import Data.Generic (gShow)
import Prelude

apiUrl :: String
apiUrl = "http://localhost:3000"

styleClass = HP.class_ <<< ClassName

placeholder length width =
  img [ HP.src ("http://via.placeholder.com/" <> gShow length <> "x" <> gShow width) ]

