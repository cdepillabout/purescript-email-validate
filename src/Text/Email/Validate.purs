module Text.Email.Validate
    ( isValid
    , validate
    , emailAddress
    , canonicalizeEmail
    , runEmailParser
    , module Text.Email.Parser
    )
where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Text.Email.Parser (EmailAddress(..), addrSpec, domainPart, localPart, toString)
import StringParser (ParseError, runParser)

-- | Smart constructor for an email address
emailAddress :: String -> Maybe EmailAddress
emailAddress emailString =
    case validate emailString of
         Left _ -> Nothing
         Right email -> Just email

-- | Checks that an email is valid and returns a version of it
--   where comments and whitespace have been removed.
canonicalizeEmail :: String -> Maybe String
canonicalizeEmail = map toString <<< emailAddress

-- | Validates whether a particular string is an email address
--   according to RFC5322.
isValid :: String -> Boolean
isValid emailString =
    case validate emailString of
         Left _ -> false
         Right _ -> true

-- | If you want to find out *why* a particular string is not
--   an email address, use this.
validate :: String -> Either String EmailAddress
validate = lmap show <<< runEmailParser

-- | Run a parser for an input string, returning either an error or a result.
runEmailParser :: String -> Either ParseError EmailAddress
runEmailParser s = runParser addrSpec s
