
module Test.Main where

import Prelude

import Control.Apply ((*>))
import Control.Monad.Aff (Aff())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Trans (lift)
import Data.Array ((!!))
import Data.Char (fromCharCode)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String (fromChar, length)
import Test.Spec (Spec(), describe, pending, it)
import Test.Spec.Runner (Process(), run)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Text.Parsing.StringParser (Parser(), ParseError(..), Pos(), PosString(), unParser)

import Text.Email.Parser

-- | Run a parser for an input string, returning either an error or a result.
runParser :: forall e a. Parser a -> String -> Aff (console :: CONSOLE | e) (Either ParseError a)
runParser p s = unParser p { str: s, pos: 0 } errorHandler successHandler
  where
    errorHandler :: Pos -> ParseError -> Aff (console :: CONSOLE | e) (Either ParseError a)
    errorHandler _ err = pure $ Left err

    successHandler :: a -> PosString -> Aff (console :: CONSOLE | e) (Either ParseError a)
    successHandler a posString = do
        liftEff $ log $ "string length: " <> show (length posString.str)
        liftEff $ log $ "position: " <> show posString.pos
        if length posString.str == posString.pos
            then pure $ Right a
            else pure $ Left $ ParseError "could not parse the whole email string"

main :: forall e . Eff (console :: CONSOLE, process :: Process | e) Unit
main = run [consoleReporter] do
    describe "email validation" do
        traverse_ runUnitTests units


runUnitTests :: forall e . EmailUnitTest -> Spec (console :: CONSOLE | e) Unit
runUnitTests emailUnitTest@(EmailUnitTest {email: e, shouldPass: result, errorString: err }) = do
    let msg = if result
                then "email address " <> e <> " is good"
                else "email address " <> e <> " is bad because: " <> err
    it msg do
        result <- doUnitTest
        result `shouldEqual` true
  where
    doUnitTest :: Aff (console :: CONSOLE | e) Boolean
    doUnitTest = do
        eitherParseResult <- runParser addrSpec e
        case eitherParseResult of
             Left error -> do
                 liftEff $ log $ "ERROR parsing " <> e <> ": " <> show error
                 pure (not result)
             Right emailAddress -> do
                 liftEff $ log $ show emailAddress
                 pure result

newtype EmailUnitTest = EmailUnitTest { email :: String
                                      , shouldPass :: Boolean
                                      , errorString :: String
                                      }

mkUnitTest :: String -> Boolean -> String -> EmailUnitTest
mkUnitTest email shouldPass errorString =
    EmailUnitTest ({ email : email, shouldPass : shouldPass, errorString : errorString })

-- | A big array of email addresses and whether or not they are vaild.
units :: Array EmailUnitTest
units = [ mkUnitTest "first.last@example.com" true ""
        , mkUnitTest "1234567890123456789012345678901234567890123456789012345678901234@example.com" true ""
        , mkUnitTest "\"first last\"@example.com" true ""
        , mkUnitTest "\"first\\\"last\"@example.com" true ""
        , mkUnitTest "first\\@last@example.com" false "Escaping can only happen within a quoted string"
        , mkUnitTest "\"first@last\"@example.com" true ""
        , mkUnitTest "\"first\\\\last\"@example.com" true ""
        , mkUnitTest "x@x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x234" true ""
        , mkUnitTest "123456789012345678901234567890123456789012345678901234567890@12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.123456789012345678901234567890123456789012345678901234567890123.example.com" true ""
        , mkUnitTest "first.last@[12.34.56.78]" true ""
        , mkUnitTest "first.last@[IPv6:::12.34.56.78]" true ""
        , mkUnitTest "first.last@[IPv6:1111:2222:3333::4444:12.34.56.78]" true ""
        , mkUnitTest "first.last@[IPv6:1111:2222:3333:4444:5555:6666:12.34.56.78]" true ""
        , mkUnitTest "first.last@[IPv6:::1111:2222:3333:4444:5555:6666]" true ""
        , mkUnitTest "first.last@[IPv6:1111:2222:3333::4444:5555:6666]" true ""
        , mkUnitTest "first.last@[IPv6:1111:2222:3333:4444:5555:6666::]" true ""
        , mkUnitTest "first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777:8888]" true ""
        , mkUnitTest "first.last@x23456789012345678901234567890123456789012345678901234567890123.example.com" true ""
        , mkUnitTest "first.last@1xample.com" true ""
        , mkUnitTest "first.last@123.example.com" true ""
        , mkUnitTest "first.last" false "No @"
        , mkUnitTest ".first.last@example.com" false "Local part starts with a dot"
        , mkUnitTest "first.last.@example.com" false "Local part ends with a dot"
        , mkUnitTest "first..last@example.com" false "Local part has consecutive dots"
        , mkUnitTest "\"first\"last\"@example.com" false "Local part contains unescaped excluded characters"
        , mkUnitTest "\"first\\last\"@example.com" true "Any character can be escaped in a quoted string"
        , mkUnitTest "\"\"\"@example.com" false "Local part contains unescaped excluded characters"
        , mkUnitTest "\"\\\"@example.com" false "Local part cannot end with a backslash"
        , mkUnitTest "first\\\\@last@example.com" false "Local part contains unescaped excluded characters"
        , mkUnitTest "first.last@" false "No domain"
        , mkUnitTest "\"Abc\\@def\"@example.com" true ""
        , mkUnitTest "\"Fred\\ Bloggs\"@example.com" true ""
        , mkUnitTest "\"Joe.\\\\Blow\"@example.com" true ""
        , mkUnitTest "\"Abc@def\"@example.com" true ""
        , mkUnitTest "\"Fred Bloggs\"@example.com" true ""
        , mkUnitTest "user+mailbox@example.com" true ""
        , mkUnitTest "customer/department=shipping@example.com" true ""
        , mkUnitTest "$A12345@example.com" true ""
        , mkUnitTest "!def!xyz%abc@example.com" true ""
        , mkUnitTest "_somename@example.com" true ""
        , mkUnitTest "dclo@us.ibm.com" true ""
        , mkUnitTest "abc\\@def@example.com" false "This example from RFC3696 was corrected in an erratum"
        , mkUnitTest "abc\\\\@example.com" false "This example from RFC3696 was corrected in an erratum"
        , mkUnitTest "peter.piper@example.com" true ""
        , mkUnitTest "Doug\\ \\\"Ace\\\"\\ Lovell@example.com" false "Escaping can only happen in a quoted string"
        , mkUnitTest "\"Doug \\\"Ace\\\" L.\"@example.com" true ""
        , mkUnitTest "abc@def@example.com" false "Doug Lovell says this should fail"
        , mkUnitTest "abc\\\\@def@example.com" false "Doug Lovell says this should fail"
        , mkUnitTest "abc\\@example.com" false "Doug Lovell says this should fail"
        , mkUnitTest "@example.com" false "No local part"
        , mkUnitTest "doug@" false "Doug Lovell says this should fail"
        , mkUnitTest "\"qu@example.com" false "Doug Lovell says this should fail"
        , mkUnitTest "ote\"@example.com" false "Doug Lovell says this should fail"
        , mkUnitTest ".dot@example.com" false "Doug Lovell says this should fail"
        , mkUnitTest "dot.@example.com" false "Doug Lovell says this should fail"
        , mkUnitTest "two..dot@example.com" false "Doug Lovell says this should fail"
        , mkUnitTest "\"Doug \"Ace\" L.\"@example.com" false "Doug Lovell says this should fail"
        , mkUnitTest "Doug\\ \\\"Ace\\\"\\ L\\.@example.com" false "Doug Lovell says this should fail"
        , mkUnitTest "hello world@example.com" false "Doug Lovell says this should fail"
        , mkUnitTest "gatsby@f.sc.ot.t.f.i.tzg.era.l.d." false "Doug Lovell says this should fail"
        , mkUnitTest "test@example.com" true ""
        , mkUnitTest "TEST@example.com" true ""
        , mkUnitTest "1234567890@example.com" true ""
        , mkUnitTest "test+test@example.com" true ""
        , mkUnitTest "test-test@example.com" true ""
        , mkUnitTest "t*est@example.com" true ""
        , mkUnitTest "+1~1+@example.com" true ""
        , mkUnitTest "{_test_}@example.com" true ""
        , mkUnitTest "\"[[ test ]]\"@example.com" true ""
        , mkUnitTest "test.test@example.com" true ""
        , mkUnitTest "\"test.test\"@example.com" true ""
        , mkUnitTest "test.\"test\"@example.com" true "Obsolete form, but documented in RFC2822"
        , mkUnitTest "\"test@test\"@example.com" true ""
        , mkUnitTest "test@123.123.123.x123" true ""
        , mkUnitTest "test@[123.123.123.123]" true ""
        , mkUnitTest "test@example.example.com" true ""
        , mkUnitTest "test@example.example.example.com" true ""
        , mkUnitTest "test.example.com" false ""
        , mkUnitTest "test.@example.com" false ""
        , mkUnitTest "test..test@example.com" false ""
        , mkUnitTest ".test@example.com" false ""
        , mkUnitTest "test@test@example.com" false ""
        , mkUnitTest "test@@example.com" false ""
        , mkUnitTest "-- test --@example.com" false "No spaces allowed in local part"
        , mkUnitTest "[test]@example.com" false "Square brackets only allowed within quotes"
        , mkUnitTest "\"test\\test\"@example.com" true "Any character can be escaped in a quoted string"
        , mkUnitTest "\"test\"test\"@example.com" false "Quotes cannot be nested"
        , mkUnitTest "()[]\\;:,><@example.com" false "Disallowed Characters"
        , mkUnitTest "test@." false "Dave Child says so"
        , mkUnitTest "test@example." false "Dave Child says so"
        , mkUnitTest "test@.org" false "Dave Child says so"
        , mkUnitTest "test@[123.123.123.123" false "Dave Child says so"
        , mkUnitTest "test@123.123.123.123]" false "Dave Child says so"
        , mkUnitTest "NotAnEmail" false "Phil Haack says so"
        , mkUnitTest "@NotAnEmail" false "Phil Haack says so"
        , mkUnitTest "\"test\\\\blah\"@example.com" true ""
        , mkUnitTest "\"test\\blah\"@example.com" true "Any character can be escaped in a quoted string"
        , mkUnitTest "\"test\\\rblah\"@example.com" true "Quoted string specifically excludes carriage returns unless escaped"
        , mkUnitTest "\"test\rblah\"@example.com" false "Quoted string specifically excludes carriage returns"
        , mkUnitTest "\"test\\\"blah\"@example.com" true ""
        , mkUnitTest "\"test\"blah\"@example.com" false "Phil Haack says so"
        , mkUnitTest "customer/department@example.com" true ""
        , mkUnitTest "_Yosemite.Sam@example.com" true ""
        , mkUnitTest "~@example.com" true ""
        , mkUnitTest ".wooly@example.com" false "Phil Haack says so"
        , mkUnitTest "wo..oly@example.com" false "Phil Haack says so"
        , mkUnitTest "pootietang.@example.com" false "Phil Haack says so"
        , mkUnitTest ".@example.com" false "Phil Haack says so"
        , mkUnitTest "\"Austin@Powers\"@example.com" true ""
        , mkUnitTest "Ima.Fool@example.com" true ""
        , mkUnitTest "\"Ima.Fool\"@example.com" true ""
        , mkUnitTest "\"Ima Fool\"@example.com" true ""
        , mkUnitTest "Ima Fool@example.com" false "Phil Haack says so"
        , mkUnitTest "phil.h\\@\\@ck@haacked.com" false "Escaping can only happen in a quoted string"
        , mkUnitTest "\"first\".\"last\"@example.com" true ""
        , mkUnitTest "\"first\".middle.\"last\"@example.com" true ""
        , mkUnitTest "\"first\\\\\"last\"@example.com" false "Contains an unescaped quote"
        , mkUnitTest "\"first\".last@example.com" true "obs-local-part form as described in RFC 2822"
        , mkUnitTest "first.\"last\"@example.com" true "obs-local-part form as described in RFC 2822"
        , mkUnitTest "\"first\".\"middle\".\"last\"@example.com" true "obs-local-part form as described in RFC 2822"
        , mkUnitTest "\"first.middle\".\"last\"@example.com" true "obs-local-part form as described in RFC 2822"
        , mkUnitTest "\"first.middle.last\"@example.com" true "obs-local-part form as described in RFC 2822"
        , mkUnitTest "\"first..last\"@example.com" true "obs-local-part form as described in RFC 2822"
        , mkUnitTest "foo@[\\1.2.3.4]" false "RFC 5321 specifies the syntax for address-literal and does not allow escaping"
        , mkUnitTest "\"first\\\\\\\"last\"@example.com" true ""
        , mkUnitTest "first.\"mid\\dle\".\"last\"@example.com" true "Backslash can escape anything but must escape something"
        , mkUnitTest "Test.\r\n Folding.\r\n Whitespace@example.com" true ""
        , mkUnitTest "first\\last@example.com" false "Unquoted string must be an atom"
        , mkUnitTest "Abc\\@def@example.com" false "Was incorrectly given as a valid address in the original RFC3696"
        , mkUnitTest "Fred\\ Bloggs@example.com" false "Was incorrectly given as a valid address in the original RFC3696"
        , mkUnitTest "Joe.\\\\Blow@example.com" false "Was incorrectly given as a valid address in the original RFC3696"
        , mkUnitTest "\"test\\\r\n blah\"@example.com" false "Folding white space can\'t appear within a quoted pair"
        , mkUnitTest "\"test\r\n blah\"@example.com" true "This is a valid quoted string with folding white space"
        , mkUnitTest "{^c\\@**Dog^}@cartoon.com" false "This is a throwaway example from Doug Lovell\'s article. Actually it\'s not a valid address."
        , mkUnitTest "(foo)cal(bar)@(baz)iamcal.com(quux)" true "A valid address containing comments"
        , mkUnitTest "cal@iamcal(woo).(yay)com" true "A valid address containing comments"
        , mkUnitTest "cal(woo(yay)hoopla)@iamcal.com" true "A valid address containing comments"
        , mkUnitTest "cal(foo\\@bar)@iamcal.com" true "A valid address containing comments"
        , mkUnitTest "cal(foo\\)bar)@iamcal.com" true "A valid address containing comments and an escaped parenthesis"
        , mkUnitTest "cal(foo(bar)@iamcal.com" false "Unclosed parenthesis in comment"
        , mkUnitTest "cal(foo)bar)@iamcal.com" false "Too many closing parentheses"
        , mkUnitTest "cal(foo\\)@iamcal.com" false "Backslash at end of comment has nothing to escape"
        , mkUnitTest "first().last@example.com" true "A valid address containing an empty comment"
        , mkUnitTest "first.(\r\n middle\r\n )last@example.com" true "Comment with folding white space"
        , mkUnitTest "first(12345678901234567890123456789012345678901234567890)last@(1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890)example.com" false "Too long with comments, not too long without"
        , mkUnitTest "first(Welcome to\r\n the (\"wonderful\" (!)) world\r\n of email)@example.com" true "Silly example from my blog post"
        , mkUnitTest "pete(his account)@silly.test(his host)" true "Canonical example from RFC5322"
        , mkUnitTest "c@(Chris\'s host.)public.example" true "Canonical example from RFC5322"
        , mkUnitTest "jdoe@machine(comment).  example" true "Canonical example from RFC5322"
        , mkUnitTest "1234   @   local(blah)  .machine .example" true "Canonical example from RFC5322"
        , mkUnitTest "first(middle)last@example.com" false "Can\'t have a comment or white space except at an element boundary"
        , mkUnitTest "first(abc.def).last@example.com" true "Comment can contain a dot"
        , mkUnitTest "first(a\"bc.def).last@example.com" true "Comment can contain double quote"
        , mkUnitTest "first.(\")middle.last(\")@example.com" true "Comment can contain a quote"
        , mkUnitTest "first(abc(\"def\".ghi).mno)middle(abc(\"def\".ghi).mno).last@(abc(\"def\".ghi).mno)example(abc(\"def\".ghi).mno).(abc(\"def\".ghi).mno)com(abc(\"def\".ghi).mno)" false "Can\'t have comments or white space except at an element boundary"
        , mkUnitTest "first(abc\\(def)@example.com" true "Comment can contain quoted-pair"
        , mkUnitTest "first.last@x(1234567890123456789012345678901234567890123456789012345678901234567890).com" true "Label is longer than 63 octets, but not with comment removed"
        , mkUnitTest "a(a(b(c)d(e(f))g)h(i)j)@example.com" true ""
        , mkUnitTest "a(a(b(c)d(e(f))g)(h(i)j)@example.com" false "Braces are not properly matched"
        , mkUnitTest "name.lastname@domain.com" true ""
        , mkUnitTest ".@" false ""
        , mkUnitTest "@bar.com" false ""
        , mkUnitTest "@@bar.com" false ""
        , mkUnitTest "a@bar.com" true ""
        , mkUnitTest "aaa.com" false ""
        , mkUnitTest "aaa@.com" false ""
        , mkUnitTest "aaa@.123" false ""
        , mkUnitTest "aaa@[123.123.123.123]" true ""
        , mkUnitTest "aaa@[123.123.123.123]a" false "extra data outside ip"
        , mkUnitTest "a@bar.com." false ""
        , mkUnitTest "a-b@bar.com" true ""
        , mkUnitTest "+@b.c" true "TLDs can be any length"
        , mkUnitTest "+@b.com" true ""
        , mkUnitTest "-@..com" false ""
        , mkUnitTest "-@a..com" false ""
        , mkUnitTest "a@b.co-foo.uk" true ""
        , mkUnitTest "\"hello my name is\"@stutter.com" true ""
        , mkUnitTest "\"Test \\\"Fail\\\" Ing\"@example.com" true ""
        , mkUnitTest "valid@special.museum" true ""
        , mkUnitTest "shaitan@my-domain.thisisminekthx" true "Disagree with Paul Gregg here"
        , mkUnitTest "test@...........com" false "......"
        , mkUnitTest "\"Joe\\\\Blow\"@example.com" true ""
        , mkUnitTest "Invalid \\\n Folding \\\n Whitespace@example.com" false "This isn\'t FWS so Dominic Sayers says it\'s invalid"
        , mkUnitTest "HM2Kinsists@(that comments are allowed)this.is.ok" true ""
        , mkUnitTest "user%uucp!path@somehost.edu" true ""
        , mkUnitTest "\"first(last)\"@example.com" true ""
        , mkUnitTest " \r\n (\r\n x \r\n ) \r\n first\r\n ( \r\n x\r\n ) \r\n .\r\n ( \r\n x) \r\n last \r\n (  x \r\n ) \r\n @example.com" true ""
        , mkUnitTest "test.\r\n \r\n obs@syntax.com" true "obs-fws allows multiple lines"
        , mkUnitTest "test. \r\n \r\n obs@syntax.com" true "obs-fws allows multiple lines (test 2: space before break)"
        , mkUnitTest "test.\r\n\r\n obs@syntax.com" false "obs-fws must have at least one WSP per line"
        , mkUnitTest ("\"null \\" <> fromChar (fromCharCode 0) <> "\"@char.com") true "can have escaped null character"
        , mkUnitTest ("\"null " <> fromChar (fromCharCode 0) <> "\"@char.com") false "cannot have unescaped null character"
        ]
