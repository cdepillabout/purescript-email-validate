
module Text.Email.Parser
    -- ( -- addrSpec
    --, localPart
    --, domainPart
    -- EmailAddress()
    --, toByteString
    -- )
where

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Data.Char (fromCharCode)
import Data.Foldable (fold, intercalate)
import Data.Generic (Generic, gEq)
import Data.List (List(), concat)
import Data.String (contains, fromChar)
-- import Text.Parsing.Parser ()
import Text.Parsing.StringParser (Parser())
import Text.Parsing.StringParser.Combinators (many, many1, optional, sepBy1)
import Text.Parsing.StringParser.String (char, satisfy)

foreign import undefined :: forall a . a

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
    return (EmailAddress {localPart: l, domainPart: d})

local :: EmailParser String
local = dottedAtoms

domain :: EmailParser String
domain = dottedAtoms <|> domainLiteral

dottedAtoms :: EmailParser String
dottedAtoms = intercalate "." <$> inner
  where
    inner :: EmailParser (List String)
    inner = do
        void $ optional commentOrWhiteSpace
        ret <- (atom <|> quotedString) `sepBy1` char '.'
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
skipMany p = skipMany1 p <|> return unit

-- | Skip at least one instance of a phrase.
skipMany1 :: forall a . EmailParser a -> EmailParser Unit
skipMany1 p = do
    void p
    void $ skipMany p
    return unit

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
isQuotedText x = inClass (fromChar (fromCharCode 33)) x
              || inClassRange (fromCharCode 35) (fromCharCode 91) x
              || inClassRange (fromCharCode 93) (fromCharCode 126) x
              || isObsNoWsCtl x

quotedPair :: EmailParser String
quotedPair = do
    c <- what
    pure $ "\\" <> fromChar c
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
    skipMany $ commentContent <|> whiteSpaceOrNewLine
    void $ char ')'
    pure unit
  where
    commentContent :: EmailParser Unit
    -- commentContent = skipWhile1 isCommentText <|> void quotedPair <|> comment
    commentContent = skipWhile1 isCommentText <|> void quotedPair

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
inClass string char = fromChar char `contains` string

inClassRange :: Char -> Char -> Char -> Boolean
inClassRange start end c = c >= start && c <= end

atom :: EmailParser String
atom = takeWhile1 isAtomText
  where
    isAtomText :: Char -> Boolean
    isAtomText x = isAlphaNum x || inClass "!#$%&'*+/=?^_`{|}~-" x


takeWhile1 :: (Char -> Boolean) -> EmailParser String
takeWhile1 f = fold <<< map fromChar <$> (many1 $ satisfy f)

-- instance Show EmailAddress where
--     show = show . toByteString

-- instance Read EmailAddress where
--     readListPrec = Read.readListPrecDefault
--     readPrec = Read.parens (do
--         bs <- Read.readPrec
--         case parseOnly (addrSpec <* endOfInput) bs of
--             Left  _ -> Read.pfail
--             Right a -> return a)

-- -- | Converts an email address back to a ByteString
-- toByteString :: EmailAddress -> ByteString
-- toByteString (EmailAddress l d) = BS.concat [l, BS.singleton '@', d]

-- -- | Extracts the local part of an email address.
-- localPart :: EmailAddress -> ByteString
-- localPart (EmailAddress l _) = l

-- -- | Extracts the domain part of an email address.
-- domainPart :: EmailAddress -> ByteString
-- domainPart (EmailAddress _ d) = d

-- -- | A parser for email addresses.
-- addrSpec :: Parser EmailAddress
-- addrSpec = do
--     l <- local
--     _ <- char '@'
--     d <- domain
--     return (EmailAddress l d)

-- local :: Parser ByteString
-- local = dottedAtoms

-- domain :: Parser ByteString
-- domain = dottedAtoms <|> domainLiteral

-- dottedAtoms :: Parser ByteString
-- dottedAtoms = BS.intercalate (BS.singleton '.') <$>
--         between1 (optional commentOrWhiteSpace) (atom <|> quotedString) `sepBy1` char '.'

-- atom :: Parser ByteString
-- atom = takeWhile1 isAtomText

-- isAtomText :: Char -> Bool
-- isAtomText x = isAlphaNum x || inClass "!#$%&'*+/=?^_`{|}~-" x

-- domainLiteral :: Parser ByteString
-- domainLiteral =
--     (BS.cons '[' . flip BS.snoc ']' . BS.concat) <$>
--         between (optional commentOrWhiteSpace *> char '[') (char ']' <* optional commentOrWhiteSpace)
--             (many (optional whiteSpaceOrNewLine >> takeWhile1 isDomainText) <* optional whiteSpaceOrNewLine)

-- isDomainText :: Char -> Bool
-- isDomainText x = inClass "\33-\90\94-\126" x || isObsNoWsCtl x

-- quotedString :: Parser ByteString
-- quotedString =
--     (\x -> BS.concat [BS.singleton '"', BS.concat x, BS.singleton '"']) <$>
--         between (char '"') (char '"')
--             (many (optional whiteSpaceOrNewLine >> quotedContent) <* optional whiteSpaceOrNewLine)

-- quotedContent :: Parser ByteString
-- quotedContent = takeWhile1 isQuotedText <|> quotedPair

-- isQuotedText :: Char -> Bool
-- isQuotedText x = inClass "\33\35-\91\93-\126" x || isObsNoWsCtl x

-- quotedPair :: Parser ByteString
-- quotedPair = (BS.cons '\\' . BS.singleton) <$> (char '\\' *> (vchar <|> whiteSpace <|> lf <|> cr <|> obsNoWsCtl <|> nullChar))

-- commentOrWhiteSpace :: Parser ()
-- commentOrWhiteSpace = skipMany (comment <|> whiteSpaceOrNewLine)

-- whiteSpaceOrNewLine :: Parser ()
-- whiteSpaceOrNewLine = void (whiteSpace1 >> optional (crlf >> whiteSpace1)) <|> (skipMany1 (crlf >> whiteSpace1))

-- between :: Applicative f => f l -> f r -> f a -> f a
-- between l r x = l *> x <* r

-- between1 :: Applicative f => f lr -> f a -> f a
-- between1 lr x = lr *> x <* lr

-- comment :: Parser ()
-- comment = between (char '(') (char ')') $ skipMany (void commentContent <|> whiteSpaceOrNewLine)

-- commentContent :: Parser ()
-- commentContent = skipWhile1 isCommentText <|> void quotedPair <|> comment

-- isCommentText :: Char -> Bool
-- isCommentText x = inClass "\33-\39\42-\91\93-\126" x || isObsNoWsCtl x

-- nullChar :: Parser Char
-- nullChar = char '\0'

-- skipWhile1 :: (Char -> Bool) -> Parser()
-- skipWhile1 x = satisfy x >> skipWhile x

-- whiteSpace1 :: Parser ()
-- whiteSpace1 = skipWhile1 isWsp

-- whiteSpace :: Parser Char
-- whiteSpace = satisfy isWsp

-- isWsp :: Char -> Bool
-- isWsp x = x == ' ' || x == '\t'

-- isAlphaNum :: Char -> Bool
-- isAlphaNum x = isDigit x || isAlpha_ascii x

-- cr :: Parser Char
-- cr = char '\r'

-- lf :: Parser Char
-- lf = char '\n'

-- crlf :: Parser ()
-- crlf = void $ cr >> lf

-- isVchar :: Char -> Bool
-- isVchar = inClass "\x21-\x7e"

-- vchar :: Parser Char
-- vchar = satisfy isVchar

-- isObsNoWsCtl :: Char -> Bool
-- isObsNoWsCtl = inClass "\1-\8\11-\12\14-\31\127"

-- obsNoWsCtl :: Parser Char
-- obsNoWsCtl = satisfy isObsNoWsCtl

