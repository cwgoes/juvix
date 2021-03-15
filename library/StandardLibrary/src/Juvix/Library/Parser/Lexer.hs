module Juvix.Library.Parser.Lexer
  ( spacer,
    spaceLiner,
    skipLiner,
    parens,
    brackets,
    curly,
    many1H,
    sepBy1H,
    sepBy1HFinal,
    maybeParend,
    emptyCheck,
    eatSpaces,
    integer,
  )
where

-- space,

import qualified Data.ByteString.Char8 as Char8
import qualified Data.List.NonEmpty as NonEmpty
import Data.Word8 (isDigit, isSpace)
import Juvix.Library.Parser.Internal (Parser)
import qualified Juvix.Library.Parser.Token as Tok
import Protolude
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Byte as P
import Prelude (fail)

emptyCheck :: Word8 -> Bool
emptyCheck x = isSpace x || x == Tok.newLine

spacer :: Parser p -> Parser p
spacer p = P.takeWhileP (Just "spacer") isSpace *> p

spaceLiner :: Parser p -> Parser p
spaceLiner p = do
  p <* P.takeWhileP (Just "space liner") emptyCheck

eatSpaces :: Parser p -> Parser p
eatSpaces p = P.takeWhileP (Just "eat spaces") emptyCheck *> p

between :: Word8 -> Parser p -> Word8 -> Parser p
between fst p end = skipLiner fst *> spaceLiner p <* P.satisfy (== end)

parens :: Parser p -> Parser p
parens p = between Tok.openParen p Tok.closeParen

brackets :: Parser p -> Parser p
brackets p = between Tok.openBracket p Tok.closeBracket

curly :: Parser p -> Parser p
curly p = between Tok.openCurly p Tok.closeCurly

many1H :: Parser a -> Parser (NonEmpty a)
many1H = fmap NonEmpty.fromList . P.some

-- | 'sepBy1HFinal' is like 'sepBy1H' but also tries to parse a last separator
sepBy1HFinal :: Parser a -> Parser s -> Parser (NonEmpty a)
sepBy1HFinal parse sep = sepBy1H parse sep <* P.optional sep

sepBy1 :: Parser a -> Parser s -> Parser [a]
sepBy1 p sep = liftA2 (:) p (many (P.try $ sep *> p))

sepBy1H :: Parser a -> Parser s -> Parser (NonEmpty a)
sepBy1H parse sep = NonEmpty.fromList <$> sepBy1 parse sep

skipLiner :: Word8 -> Parser ()
skipLiner p = spaceLiner (P.skipCount 1 (P.char p))

maybeParend :: Parser a -> Parser a
maybeParend p = p <|> parens p

integer :: Parser Integer
integer = do
  digits <- P.takeWhileP (Just "digits") isDigit
  case Char8.readInteger digits of
    Just (x, _) -> pure x
    Nothing -> fail $ "didn't parse an int: " <> toS digits
