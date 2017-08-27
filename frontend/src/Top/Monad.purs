module Top.Monad where

import Prelude

import Control.Applicative.Free (FreeAp)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Free (Free, foldFree, liftF)
import Data.JSDate (LOCALE)
import Halogen.Aff (HalogenEffects)
import Network.HTTP.Affjax (AJAX)

type Top = TopM TopEffects

type TopEffects = HalogenEffects TopRawEffects

data TopF eff a
  = Aff (Aff eff a)

newtype TopM eff a = TopM (Free (TopF eff) a)

unTopM :: forall eff. TopM eff ~> Free (TopF eff)
unTopM (TopM a) = a

derive newtype instance functorTopM :: Functor (TopM eff)
derive newtype instance applyTopM :: Apply (TopM eff)
derive newtype instance applicativeTopM :: Applicative (TopM eff)
derive newtype instance bindTopM :: Bind (TopM eff)
derive newtype instance monadTopM :: Monad (TopM eff)


newtype TopA eff a = TopA (FreeAp (TopM eff) a)

derive newtype instance functorTopA :: Functor (TopA eff)
derive newtype instance applyTopA :: Apply (TopA eff)
derive newtype instance applicativeTopA :: Applicative (TopA eff)

instance monadEffTopM :: MonadEff eff (TopM eff) where
  liftEff = TopM <<< liftF <<< Aff <<< liftEff

instance monadAffTopM :: MonadAff eff (TopM eff) where
  liftAff = TopM <<< liftF <<< Aff

type TopRawEffects =
  ( ajax :: AJAX
  , random :: RANDOM
  , console :: CONSOLE
  , now :: NOW
  , timer :: TIMER
  , locale :: LOCALE
  )

runTop :: Top ~> Aff TopEffects
runTop = foldFree go <<< unTopM
  where
    go :: TopF TopEffects ~> Aff TopEffects
    go = case _ of
      Aff aff -> aff
