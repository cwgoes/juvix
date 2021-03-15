module Juvix.Frontend.Types.Located where

import Juvix.Library
import Juvix.Library.Parser (Parser)
import qualified Text.Megaparsec as P

data Loc
  = NoLoc
  | Loc {line :: Int, col :: Int}
  deriving (Eq, Show, Ord, Generic)

data Located a = Located {located :: Loc, locVal :: a}
  deriving (Generic, Show)

instance Functor Located where
  fmap f (Located l v) = Located l (f v)

instance Eq a => Eq (Located a) where
  (==) l1 l2 = locVal l1 == locVal l2

instance Ord a => Ord (Located a) where
  compare l1 l2 = locVal l1 `compare` locVal l2

-- | Annotate something with no-location location information.
noLoc :: a -> Located a
noLoc = Located NoLoc

location :: Parser Loc
location = do
  srcPos <- P.getSourcePos
  let line = P.unPos $ P.sourceLine srcPos
      col = P.unPos $ P.sourceColumn srcPos
  pure $ Loc line col

mkLocated :: Parser a -> Parser (Located a)
mkLocated p = Located <$> location <*> p
