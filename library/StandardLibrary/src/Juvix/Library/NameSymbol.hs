module Juvix.Library.NameSymbol
  ( module Juvix.Library.NameSymbol,
    fromString,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import Data.String (IsString (..))
import qualified Data.Text as Text
import Juvix.Library
import qualified Juvix.Library.Parser.Token as Tok
import qualified Prelude (foldr1)

type T = NonEmpty Symbol

type Base = Symbol

type Mod = [Symbol]

toSymbol :: T -> Symbol
toSymbol =
  Prelude.foldr1 (\x acc -> x <> "." <> acc)

fromSymbol :: Symbol -> T
fromSymbol =
  NonEmpty.fromList . fmap internText . handleInfix . Text.splitOn "." . textify

fromText :: Text -> T
fromText = fromSymbol . internText

-- TODO ∷ make this a fold!?
handleInfix :: [Text] -> [Text]
handleInfix [] = []
handleInfix (x : xs) = rec' (x : xs) ""
  where
    rec' ("" : xs) currentInfixSymbol =
      rec' xs (Text.snoc currentInfixSymbol '.')
    rec' (x : xs) build
      -- case 1)
      | Tok.validInfixSymbol (Tok.charToWord8 (Text.head x)) =
        rec' xs (build <> Text.cons '.' x)
      | Text.null build =
        x : rec' xs build
      | otherwise =
        -- case 3)
        -- we must tail x, as we add an extra .
        -- at the start of every infix symbol.
        -- we do this because even a symbol like
        -- "-" triggers case 1) which adds a '.'
        -- to the front even when it shouldn't!
        -- this is the correct behavior IFF we
        -- are in the middle of a infix symbol!
        Text.tail build : x : xs
    rec' [] "" =
      []
    rec' [] acc =
      -- see case 3)
      [Text.tail acc]

instance IsString T where
  fromString = fromSymbol . intern

prefixOf :: T -> T -> Bool
prefixOf smaller larger =
  case takePrefixOfInternal smaller larger of
    Just _ -> True
    Nothing -> False

takePrefixOf :: T -> T -> Maybe T
takePrefixOf smaller larger =
  case takePrefixOfInternal smaller larger of
    Just [] -> Nothing
    Nothing -> Nothing
    Just (x : xs) -> Just (x :| xs)

takePrefixOfInternal :: T -> T -> Maybe [Symbol]
takePrefixOfInternal (s :| smaller) (b :| bigger)
  | b == s = recurse smaller bigger
  | otherwise = Nothing
  where
    recurse [] ys = Just ys
    recurse _ [] = Nothing
    recurse (x : xs) (y : ys)
      | x == y = recurse xs ys
      | otherwise = Nothing

cons :: Symbol -> T -> T
cons = NonEmpty.cons

hd :: T -> Symbol
hd = NonEmpty.head

qualify :: Foldable t => t Symbol -> T -> T
qualify m n = foldr cons n m

qualify1 :: Foldable t => t Symbol -> Base -> T
qualify1 m b = qualify m (b :| [])

qualified :: T -> Bool
qualified (_ :| xs) = not $ null xs

split :: T -> (Mod, Base)
split n = (NonEmpty.init n, NonEmpty.last n)

mod :: T -> Mod
mod = fst . split

base :: T -> Base
base = snd . split

applyBase :: (Base -> Base) -> T -> T
applyBase f n = let (m, b) = split n in qualify1 m (f b)
