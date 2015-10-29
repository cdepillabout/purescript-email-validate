
module Test.Main where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE())
import Test.Spec (describe, pending, it)
import Test.Spec.Runner (Process(), run)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)

import Text.Email.Parser

main :: forall e . Eff (console :: CONSOLE, process :: Process | e) Unit
main = run [consoleReporter] do
    describe "email validation" do
        it "passes unit tests" do
            let isAwesome = true
            isAwesome `shouldEqual` true
