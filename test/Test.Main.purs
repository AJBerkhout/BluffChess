module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Game.Chess.Move (moveSpec)
import Test.Spec (Spec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)


baseSpecs ∷ Spec Unit
baseSpecs = do
  moveSpec

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do baseSpecs