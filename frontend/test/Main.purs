module Test.Main where

import App.Data.Event
import Control.Monad.Eff.Now
import Import
import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.DateTime.Instant (toDateTime)
import Data.Maybe (Maybe(..))
import Helper.Format (formatDateTime, unformatDateTime)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

f e = isRight (decodeEvent (encodeEvent e))

mkEvent d a s e =
  Event { id: 1, name: "something", description: d, owner_id: 1, all_day: false, start_datetime: s, end_datetime: e, asset_id: a }

main :: forall eff. Eff (now :: NOW, console:: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR | eff) Unit
main = runTest do
  suite "decoding" do
    test "Event empty fields" do
      let empty_event = mkEvent Nothing Nothing Nothing Nothing
      Assert.assert "decoding a mostly-empty encode is valid" $ f empty_event
    test "Event some fields" do
      now <- liftEff (toDateTime <$> now)
      let event_with_1_date = mkEvent Nothing Nothing (formatDateTime now) Nothing
      let event_with_dates =  mkEvent Nothing Nothing (formatDateTime now) (formatDateTime now)
      let event_with_everything = mkEvent (Just "asdf") (Just "asdf") (formatDateTime now) (formatDateTime now)
      Assert.assert "decoding an encode with everything is valid" $ f event_with_everything
      Assert.assert "decoding an encode with one empty is valid" $ f event_with_1_date

    test "unformat . format DT" do
      now <- liftEff (toDateTime <$> now)
      Assert.assert "unformat . format is equivalent" $ isJust (unformatDateTime <$> formatDateTime now)
