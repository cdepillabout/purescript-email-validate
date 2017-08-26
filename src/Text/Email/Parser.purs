
module Text.Email.Parser
    ( EmailAddress(..)
    , EmailParser()
    , addrSpec
    , domainPart
    , localPart
    , toString
    )
where

import Prelude

import Control.Alt ((<|>))
import Data.Char (fromCharCode)
import Data.Foldable (fold, intercalate)
import Data.Generic (class Generic, gEq)
import Data.List (List)
import Data.String (Pattern(..), contains, fromCharArray, singleton)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (many, many1, optional, sepBy1)
import Text.Parsing.StringParser.String (char, eof, satisfy)

-- | Represents an email address.
newtype EmailAddress = EmailAddress { localPart :: String
                                    , domainPart :: String
                                    }

localPart :: EmailAddress -> String
localPart (EmailAddress email) = email.localPart

domainPart :: EmailAddress -> String
domainPart (EmailAddress email) = email.domainPart

derive instance genericEmailAddress :: Generic EmailAddress
instance showEmailAddress :: Show EmailAddress where show = toString
instance eqEmailAddress :: Eq EmailAddress where eq = gEq

-- | Converts an email address to a 'String'
toString :: EmailAddress -> String
toString email = localPart email <> "@" <> domainPart email

type EmailParser a = Parser a

-- | A parser for email addresses.
addrSpec :: EmailParser EmailAddress
addrSpec = do
    l <- local
    _ <- char '@'
    d <- domain
    _ <- eof
    pure (EmailAddress {localPart: l, domainPart: d})

local :: EmailParser String
local = dottedAtoms

domain :: EmailParser String
domain = dottedAtoms <|> domainLiteral

dottedAtoms :: EmailParser String
dottedAtoms = intercalate "." <$> inner1
  where
    inner1 :: EmailParser (List String)
    inner1 = inner2 `sepBy1` char '.'

    inner2 :: EmailParser String
    inner2 = do
        void $ optional commentOrWhiteSpace
        ret <- atom <|> quotedString
        void $ optional commentOrWhiteSpace
        pure ret

domainLiteral :: EmailParser String
domainLiteral = do
    optional commentOrWhiteSpace
    void $ char '['
    domainText <- many $ optional whiteSpaceOrNewLine *> takeWhile1 isDomainText
    optional whiteSpaceOrNewLine
    void $ char ']'
    optional commentOrWhiteSpace
    pure $ "[" <> fold domainText <> "]"

isDomainText :: Char -> Boolean
isDomainText x = inClassRange (fromCharCode 33) (fromCharCode 90) x
              || inClassRange (fromCharCode 94) (fromCharCode 126) x
              || isObsNoWsCtl x

quotedString :: EmailParser String
quotedString = (\x -> "\"" <> fold x <> "\"") <$> what
  where
    what :: EmailParser (List String)
    what = do
        void $ char '"'
        ret <- many $ optional whiteSpaceOrNewLine *> quotedContent
        void $ optional whiteSpaceOrNewLine
        void $ char '"'
        pure ret

-- | Skip many instances of a phrase.
skipMany :: forall a . EmailParser a -> EmailParser Unit
skipMany p = skipMany1 p <|> pure unit

-- | Skip at least one instance of a phrase.
skipMany1 :: forall a . EmailParser a -> EmailParser Unit
skipMany1 p = do
    void p
    void $ skipMany p
    pure unit

commentOrWhiteSpace :: EmailParser Unit
commentOrWhiteSpace = skipMany (comment <|> whiteSpaceOrNewLine)

whiteSpaceOrNewLine :: EmailParser Unit
whiteSpaceOrNewLine = p1 <|> p2
  where
    p1 :: EmailParser Unit
    p1 = whiteSpace1 *> optional (crlf *> whiteSpace1)

    p2 :: EmailParser Unit
    p2 = skipMany1 $ crlf *> whiteSpace1

quotedContent :: EmailParser String
quotedContent = takeWhile1 isQuotedText <|> quotedPair

isQuotedText :: Char -> Boolean
isQuotedText x = inClass (fromCharArray <<< pure $ fromCharCode 33) x
              || inClassRange (fromCharCode 35) (fromCharCode 91) x
              || inClassRange (fromCharCode 93) (fromCharCode 126) x
              || isObsNoWsCtl x

quotedPair :: EmailParser String
quotedPair = do
    c <- what
    pure $ "\\" <> singleton c
  where
    what :: EmailParser Char
    what = do
        void $ char '\\'
        vchar <|> whiteSpace <|> lf <|> cr <|> obsNoWsCtl <|> nullChar

isVchar :: Char -> Boolean
isVchar = inClassRange (fromCharCode 33) (fromCharCode 126)

vchar :: EmailParser Char
vchar = satisfy isVchar

comment :: EmailParser Unit
comment = do
    void $ char '('
    skipMany $ skipWhile1 isCommentText <|> void quotedPair <|> comment <|> whiteSpaceOrNewLine
    void $ char ')'
    pure unit

isCommentText :: Char -> Boolean
isCommentText x = inClassRange (fromCharCode 33) (fromCharCode 39) x
               || inClassRange (fromCharCode 42) (fromCharCode 91) x
               || inClassRange (fromCharCode 93) (fromCharCode 126) x
               || isObsNoWsCtl x

nullChar :: EmailParser Char
nullChar = char $ fromCharCode 0

skipWhile1 :: (Char -> Boolean) -> EmailParser Unit
skipWhile1 x = do
    void $ satisfy x
    void $ skipMany (satisfy x)
    pure unit

whiteSpace1 :: EmailParser Unit
whiteSpace1 = skipWhile1 isWsp

whiteSpace :: EmailParser Char
whiteSpace = satisfy isWsp

isWsp :: Char -> Boolean
isWsp x = x == ' ' || x == '\t'

isAlphaNum :: Char -> Boolean
isAlphaNum x = isDigit || isAlpha_ascii
  where
    isDigit :: Boolean
    isDigit = x >= '0' && x <= '9'

    isAlpha_ascii :: Boolean
    isAlpha_ascii = x >= 'a' && x <= 'z' || x >= 'A' && x <= 'Z'

cr :: EmailParser Char
cr = char '\r'

lf :: EmailParser Char
lf = char '\n'

crlf :: EmailParser Unit
crlf = void $ cr *> lf

isObsNoWsCtl :: Char -> Boolean
isObsNoWsCtl c = inClassRange (fromCharCode 1) (fromCharCode 8) c
              || inClassRange (fromCharCode 14) (fromCharCode 31) c
              || inClass "\11\12\127" c

obsNoWsCtl :: EmailParser Char
obsNoWsCtl = satisfy isObsNoWsCtl

inClass :: String -> Char -> Boolean
inClass string char = (Pattern $ singleton char) `contains` string

inClassRange :: Char -> Char -> Char -> Boolean
inClassRange start end c = c >= start && c <= end

atom :: EmailParser String
atom = takeWhile1 isAtomText
  where
    isAtomText :: Char -> Boolean
    isAtomText x = isAlphaNum x || inClass "!#$%&'*+/=?^_`{|}~-" x


takeWhile1 :: (Char -> Boolean) -> EmailParser String
takeWhile1 f = fold <<< map singleton <$> (many1 $ satisfy f)
