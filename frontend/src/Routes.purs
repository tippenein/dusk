module Routes where

import Import
import Data.Generic.Rep.Eq (genericEq)

data Location
  = HomeR
  | ProfileR
  | CuratorsR
  | EventsR
  | EventR Int
  | LoginR
  | AdminR
  | NotFoundR String

derive instance genericLocation :: Generic Location _

instance showLocation :: Show Location where
  show = genericShow

instance eqLocation :: Eq Location where
  eq = genericEq

data ChildAction
  = Redirect Location

data Input a
  = Goto Location a
  | Noop a
  | CheckProfile a
  | Logout a
