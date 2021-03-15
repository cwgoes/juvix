{-# LANGUAGE TypeApplications #-}

module Juvix.Library.Sexp.Parser
  ( parse,
  )
where

import qualified Data.Text.Encoding as Encoding
import Juvix.Library hiding (list)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Parser as J
import Juvix.Library.Parser.Internal (Parser, ParserError)
import qualified Juvix.Library.Sexp.Types as Sexp
import qualified Text.Megaparsec as P

-- | @parse@ parses any sexp expression into the Sexp type
parse :: ByteString -> Either ParserError Sexp.T
parse = P.parse (J.eatSpaces sexp) ""

--------------------------------------------------------------------------------
-- Sexp Main Parsers
--------------------------------------------------------------------------------
sexp :: Parser Sexp.T
sexp = J.spaceLiner (list <|> (Sexp.Atom <$> atom))

list :: Parser Sexp.T
list = do
  d <- parens (many sexp)
  case d of
    [] -> pure Sexp.Nil
    _ -> pure (foldr Sexp.Cons Sexp.Nil d)

atom :: Parser Sexp.Atom
atom = number <|> name

name :: Parser Sexp.Atom
name = do
  sym <- symbol
  pure (Sexp.A sym Nothing)

number :: Parser Sexp.Atom
number = do
  int <- J.integer
  pure (Sexp.N int Nothing)

symbol :: Parser NameSymbol.T
symbol = do
  s <-
    P.takeWhile1P
      (Just "Valid symbol")
      ( \x ->
          J.validStartSymbol x
            || J.validMiddleSymbol x
            || J.validInfixSymbol x
      )
  Encoding.decodeUtf8 s |> internText |> NameSymbol.fromSymbol |> pure

--------------------------------------------------------------------------------
-- Helpers taken from the other parser
--------------------------------------------------------------------------------

-- edited a bit from the other parser
between :: Word8 -> Parser p -> Word8 -> Parser p
between fst p end = J.skipLiner fst *> J.spaceLiner p <* J.skipLiner end

parens :: Parser p -> Parser p
parens p = between J.openParen p J.closeParen
