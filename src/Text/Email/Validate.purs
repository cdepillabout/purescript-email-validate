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
import Data.String (length)
import Text.Email.Parser (EmailAddress(..), addrSpec, domainPart, localPart, toString)
import Text.Parsing.StringParser (Parser(), ParseError(..), Pos(), PosString(), unParser)

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
runEmailParser :: forall e. String -> Either ParseError EmailAddress
runEmailParser s = unParser addrSpec { str: s, pos: 0 } errorHandler successHandler
  where
    errorHandler :: Pos -> ParseError -> Either ParseError EmailAddress
    errorHandler _ err = Left err

    successHandler :: EmailAddress -> PosString -> Either ParseError EmailAddress
    successHandler emailAddr posString = do
        if length posString.str == posString.pos
            then Right emailAddr
            else Left $ ParseError "leftover characters at end of email string"
