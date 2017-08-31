module Timer where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Timer (TIMER)

foreign import data Timeout   :: Type
foreign import data Interval  :: Type

type Milliseconds = Int
-- | main :: forall eff. Eff (console :: CONSOLE, timer :: TIMER | eff) Unit
-- | main = do
-- |   print 1
-- |   t <- timeout 10 $ do
-- |     print 3
-- |   print 2
-- | ```

foreign import timeout :: forall a eff.
          Milliseconds ->
          Eff (timer :: TIMER | eff) a ->
          Eff (timer :: TIMER | eff) Timeout

-- | ```purescript
-- | delay 1000 log "hi"
-- | ```
delay :: forall a b eff.
          Milliseconds
          -> (a -> Eff (timer :: TIMER | eff) b)
          ->  a -> Eff (timer :: TIMER | eff) Timeout
delay x cb a = timeout x $ cb a
