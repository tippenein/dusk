module Helper where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery as Q
import Data.Generic (gShow)
import Halogen.HTML (ClassName(..), img)
import Halogen.HTML.Properties (class_, src) as HP
import Halogen.HTML.Properties.ARIA as HP
import Import (Maybe(..))
import Prelude hiding (div)
import Timer (timeout)
import Top.Monad (TopEffects)
import Halogen.HTML (ClassName(ClassName), div, img)

apiUrl :: String
apiUrl = "http://localhost:3000"

col n = div [ styleClass $ "col-sm-" <> show n]

styleClass = styleClassIf true

styleClassIf b n = HP.class_ <<< ClassName $ if b then n else ""

placeholder length width =
  img [ HP.src ("http://via.placeholder.com/" <> gShow length <> "x" <> gShow width) ]


data Message
  = Success
  | Failure
  | Info
  | Warning

flashMessage :: Message -> String -> Eff TopEffects Unit
flashMessage s msg = do
  let klass = "alert-" <> msgToString s
  alert <- Q.select ".alert"
  message <- Q.select "#app-message"
  Q.addClass klass alert
  Q.display alert
  Q.appendText msg message
  t <- timeout 5500 $ do
    Q.removeClass klass alert
    Q.hide alert
    Q.clear message
    pure unit
  pure unit

msgToString :: Message -> String
msgToString Success = "success"
msgToString Info    = "info"
msgToString Warning = "warning"
msgToString Failure = "danger"


stringToMaybe :: String -> Maybe String
stringToMaybe "" = Nothing
stringToMaybe a = Just a
