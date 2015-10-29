
module Test.Main where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE())
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Test.Spec (describe, pending, it)
import Test.Spec.Runner (Process(), run)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Text.Parsing.StringParser (runParser)

import Text.Email.Parser

main :: forall e . Eff (console :: CONSOLE, process :: Process | e) Unit
main = run [consoleReporter] do
    describe "email validation" do
        it "passes unit tests" do
            case units !! 0 of
                 Just u -> doUnitTest u `shouldEqual` true
                 Nothing -> pure unit

doUnitTest :: EmailUnitTest -> Boolean
doUnitTest (EmailUnitTest {email: e, shouldPass: result, errorString: err }) =
    case runParser addrSpec e of
        Left parseError -> not result
        Right emailAddress -> result

newtype EmailUnitTest = EmailUnitTest { email :: String
                                      , shouldPass :: Boolean
                                      , errorString :: String
                                      }
units :: Array EmailUnitTest
units = [ EmailUnitTest ({ email : "first\\@last@example.com", shouldPass : false, errorString : "" })
    -- ("1234567890123456789012345678901234567890123456789012345678901234@example.com", True, ""),
        ]
