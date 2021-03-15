module Juvix.Library.Parser.Internal
  ( Parser,
    ParserError,
  )
where

import Protolude
import qualified Text.Megaparsec as P

type Parser = P.Parsec Void ByteString

--                   ^    ^
--                   |    |
-- Custom error component Type of input stream

type ParserError = P.ParseErrorBundle ByteString Void
