
module Text.Email.Parser
    ( -- addrSpec
    --, localPart
    --, domainPart
    EmailAddress()
    --, toByteString
    )
where

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Data.Foldable (fold, intercalate)
import Data.Generic (Generic, gEq)
import Data.List (List(), concat)
import Data.Maybe.Unsafe (unsafeThrow)
import Data.String (contains, fromChar)
-- import Text.Parsing.Parser ()
import Text.Parsing.StringParser (Parser())
import Text.Parsing.StringParser.Combinators (many, optional, sepBy1)
import Text.Parsing.StringParser.String (char, satisfy)

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
-- domain = dottedAtoms <|> domainLiteral
domain = unsafeThrow "error"

dottedAtoms :: EmailParser String
dottedAtoms = intercalate "." <$> what
  where
    what :: EmailParser (List String)
    what = do
        void $ optional cfws
        ret <- (atom <|> quotedString) `sepBy1` char '.'
        void $ optional cfws
        pure ret

quotedString :: EmailParser String
quotedString = (\x -> "\"" <> fold x <> "\"") <$> what
  where
    what :: EmailParser (List String)
    what = do
        void $ char '"'
        ret <- many $ optional fws *> quotedContent
        void $ optional fws
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

cfws :: EmailParser Unit
cfws = skipMany (comment <|> fws)

fws :: EmailParser Unit
fws = void something1 <|> something2
  where
    something1 :: EmailParser Unit
    something1 = wsp1 *> optional (crlf *> wsp1)

    something2 :: EmailParser Unit
    something2 = skipMany1 $ crlf *> wsp1

quotedContent :: EmailParser String
quotedContent = takeWhile1 isQuotedText <|> quotedPair

isQuotedText :: Char -> Boolean
isQuotedText x = inClass "\33\35-\91\93-\126" x || isObsNoWsCtl x

quotedPair :: EmailParser String
quotedPair = do
    c <- what
    pure $ "\\" <> fromChar c
  where
    what :: EmailParser Char
    what = do
        void $ char '\\'
        vchar <|> wsp <|> lf <|> cr <|> obsNoWsCtl <|> nullChar

isVchar :: Char -> Boolean
isVchar = inClass "\x21-\x7e"

vchar :: EmailParser Char
vchar = satisfy isVchar

comment :: EmailParser Unit
comment = do
    void $ char '('
    skipMany $ void commentContent <|> fws
    void $ char ')'
    pure unit

commentContent :: EmailParser Unit
-- commentContent = skipWhile1 isCommentText <|> void quotedPair <|> comment
commentContent = skipWhile1 isCommentText <|> void quotedPair <|> unsafeThrow "error"

isCommentText :: Char -> Boolean
-- isCommentText x = inClass "\33-\39\42-\91\93-\126" x || isObsNoWsCtl x
isCommentText x = unsafeThrow "error"

nullChar :: EmailParser Char
nullChar = char '\0'

skipWhile1 :: (Char -> Boolean) -> EmailParser Unit
skipWhile1 x = do
    void $ satisfy x
    void $ skipMany (satisfy x)
    pure unit

wsp1 :: EmailParser Unit
wsp1 = skipWhile1 isWsp

wsp :: EmailParser Char
wsp = satisfy isWsp

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

-- isVchar :: Char -> Bool
-- isVchar = inClass "\x21-\x7e"

-- vchar :: Parser Char
-- vchar = satisfy isVchar

isObsNoWsCtl :: Char -> Boolean
isObsNoWsCtl = inClass "\1-\8\11-\12\14-\31\127"

obsNoWsCtl :: EmailParser Char
obsNoWsCtl = satisfy isObsNoWsCtl

inClass :: String -> Char -> Boolean
inClass string char =
    if "-" `contains` string
       then unsafeThrow "error"
       else fromChar char `contains` string

atom :: EmailParser String
atom = takeWhile1 isAtomText
  where
    isAtomText :: Char -> Boolean
    isAtomText x = isAlphaNum x || inClass "!#$%&'*+/=?^_`{|}~-" x


takeWhile1 :: (Char -> Boolean) -> EmailParser String
takeWhile1 = unsafeThrow "error"
  where
    takeWhile :: (Char -> Boolean) -> EmailParser String
    takeWhile  = unsafeThrow "error"

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
--         between1 (optional cfws) (atom <|> quotedString) `sepBy1` char '.'

-- atom :: Parser ByteString
-- atom = takeWhile1 isAtomText

-- isAtomText :: Char -> Bool
-- isAtomText x = isAlphaNum x || inClass "!#$%&'*+/=?^_`{|}~-" x

-- domainLiteral :: Parser ByteString
-- domainLiteral =
--     (BS.cons '[' . flip BS.snoc ']' . BS.concat) <$>
--         between (optional cfws *> char '[') (char ']' <* optional cfws)
--             (many (optional fws >> takeWhile1 isDomainText) <* optional fws)

-- isDomainText :: Char -> Bool
-- isDomainText x = inClass "\33-\90\94-\126" x || isObsNoWsCtl x

-- quotedString :: Parser ByteString
-- quotedString =
--     (\x -> BS.concat [BS.singleton '"', BS.concat x, BS.singleton '"']) <$>
--         between (char '"') (char '"')
--             (many (optional fws >> quotedContent) <* optional fws)

-- quotedContent :: Parser ByteString
-- quotedContent = takeWhile1 isQuotedText <|> quotedPair

-- isQuotedText :: Char -> Bool
-- isQuotedText x = inClass "\33\35-\91\93-\126" x || isObsNoWsCtl x

-- quotedPair :: Parser ByteString
-- quotedPair = (BS.cons '\\' . BS.singleton) <$> (char '\\' *> (vchar <|> wsp <|> lf <|> cr <|> obsNoWsCtl <|> nullChar))

-- cfws :: Parser ()
-- cfws = skipMany (comment <|> fws)

-- fws :: Parser ()
-- fws = void (wsp1 >> optional (crlf >> wsp1)) <|> (skipMany1 (crlf >> wsp1))

-- between :: Applicative f => f l -> f r -> f a -> f a
-- between l r x = l *> x <* r

-- between1 :: Applicative f => f lr -> f a -> f a
-- between1 lr x = lr *> x <* lr

-- comment :: Parser ()
-- comment = between (char '(') (char ')') $ skipMany (void commentContent <|> fws)

-- commentContent :: Parser ()
-- commentContent = skipWhile1 isCommentText <|> void quotedPair <|> comment

-- isCommentText :: Char -> Bool
-- isCommentText x = inClass "\33-\39\42-\91\93-\126" x || isObsNoWsCtl x

-- nullChar :: Parser Char
-- nullChar = char '\0'

-- skipWhile1 :: (Char -> Bool) -> Parser()
-- skipWhile1 x = satisfy x >> skipWhile x

-- wsp1 :: Parser ()
-- wsp1 = skipWhile1 isWsp

-- wsp :: Parser Char
-- wsp = satisfy isWsp

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

