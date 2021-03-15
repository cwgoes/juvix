module Juvix.Library.Parser.Token
  ( charToWord8,
    wordToChr,
    dash,
    under,
    space,
    colon,
    semi,
    comma,
    hash,
    backSlash,
    quote,
    pipe,
    equals,
    at,
    question,
    dot,
    bang,
    amper,
    times,
    backtick,
    div,
    percent,
    newLine,
    openBracket,
    closeBracket,
    openParen,
    closeParen,
    openCurly,
    closeCurly,
    validStartSymbol,
    validMiddleSymbol,
    validInfixSymbol,
    validUpperSymbol,
    reservedSymbols,
    reservedWords,
    endOfLine,
  )
where

import qualified Data.Set as Set
import Data.Word8 (isDigit)
import qualified GHC.Unicode as Unicode
import Juvix.Library hiding (div, hash, maybe, option, takeWhile)

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . ord
{-# INLINE charToWord8 #-}

wordToChr :: Integral a => a -> Char
wordToChr = chr . fromIntegral

-- Hopefully this is fast!
validStartSymbol' :: Integral a => a -> Bool
validStartSymbol' = Unicode.isAlpha . wordToChr

-- Unicode.isUpper 'İ' = True!
validUpperSymbol :: Integral a => a -> Bool
validUpperSymbol = Unicode.isUpper . wordToChr

dash :: Word8
dash = charToWord8 '-'

under :: Word8
under = charToWord8 '_'

space :: Word8
space = charToWord8 ' '

colon :: Word8
colon = charToWord8 ':'

semi :: Word8
semi = charToWord8 ';'

comma :: Word8
comma = charToWord8 ','

hash :: Word8
hash = charToWord8 '#'

openParen :: Word8
openParen = charToWord8 '('

closeParen :: Word8
closeParen = charToWord8 ')'

openCurly :: Word8
openCurly = charToWord8 '{'

closeCurly :: Word8
closeCurly = charToWord8 '}'

openBracket :: Word8
openBracket = charToWord8 '['

closeBracket :: Word8
closeBracket = charToWord8 ']'

backSlash :: Word8
backSlash = charToWord8 '\\'

quote :: Word8
quote = charToWord8 '\''

pipe :: Word8
pipe = charToWord8 '|'

equals :: Word8
equals = charToWord8 '='

at :: Word8
at = charToWord8 '@'

dot :: Word8
dot = charToWord8 '.'

question :: Word8
question = charToWord8 '?'

bang :: Word8
bang = charToWord8 '!'

amper :: Word8
amper = charToWord8 '&'

times :: Word8
times = charToWord8 '*'

backtick :: Word8
backtick = charToWord8 '`'

newLine :: Word8
newLine = charToWord8 '\n'

div :: Word8
div = charToWord8 '/'

percent :: Word8
percent = charToWord8 '%'

validStartSymbol :: Word8 -> Bool
validStartSymbol w =
  validStartSymbol' w || w == under

validInfixSymbol :: Word8 -> Bool
validInfixSymbol w =
  Unicode.isSymbol (wordToChr w)
    || w == times
    || w == dash
    || w == amper
    || w == colon
    || w == div
    || w == percent
    || w == dot

validMiddleSymbol :: Word8 -> Bool
validMiddleSymbol w =
  validStartSymbol w
    || isDigit w
    || w == dash
    || w == bang
    || w == question
    || w == percent

-- check for \r or \n
endOfLine :: (Eq a, Num a) => a -> Bool
endOfLine w = w == 13 || w == 10

reservedWords :: (Ord a, IsString a) => Set a
reservedWords =
  Set.fromList
    [ "let",
      "val",
      "type",
      "case",
      "in",
      "open",
      "if",
      "cond",
      "end",
      "of",
      "begin",
      "sig",
      "mod",
      "declare",
      "where"
    ]

reservedSymbols :: (Ord a, IsString a) => Set a
reservedSymbols =
  Set.fromList
    ["=", "|", "", "--"]
