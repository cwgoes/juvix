module Juvix.Library.PrettyPrint
  ( module Juvix.Library.PrettyPrint,
    module Text.PrettyPrint.Compact
  ) where

import Text.PrettyPrint.Compact
  hiding (lparen, rparen, langle, rangle, lbrace, rbrace,
          lbracket, rbracket, squote, dquote, semi, colon,
          comma, space, dot, backslash, equals)
import Juvix.Library hiding (show)
import qualified Text.Show as Show


-- | Annotations, which can be used for e.g. syntax highlighting
type family Ann a :: *

data Prec
  -- | Outermost expression; no parens needed
  = Outer
  -- | Argument of infix function, with the same meaning as the argument of
  -- 'Text.Show.showsPrec'
  | Infix Natural
  -- | Argument of nonfix function; all non-atomic expressions need parens
  | FunArg
  deriving (Eq, Ord, Show, Generic)

type PrecReader = HasReader "prec" Prec

-- | Class for pretty-printing syntax-like types, which need to keep track of
-- the surrounding precedence level.
class Monoid (Ann a) => PrettySyntax a where
  -- | Pretty-prints a syntax value, given the current precedence value in
  -- a reader environment.
  prettyPrec' :: PrecReader m => a -> m (Doc (Ann a))
  default prettyPrec' ::
    (PrettyText a, PrecReader m) => a -> m (Doc (Ann a))
  prettyPrec' = pure . prettyText

-- | Pretty-print at the given precedence level.
prettyPrec :: PrettySyntax a => Prec -> a -> Doc (Ann a)
prettyPrec prec x =
  let MonadReader act = prettyPrec' x in
  runReader act prec

-- | Pretty-print at the initial precedence level.
prettyPrec0 :: PrettySyntax a => a -> Doc (Ann a)
prettyPrec0 = prettyPrec Outer

show :: (Monoid ann, Show a) => a -> Doc ann
show = text . Show.show

-- | Class for text-like types (e.g. messages), which don't have a concept of
-- precedence.
class Monoid (Ann a) => PrettyText a where
  -- | Pretty-print a value as human-readable text.
  prettyText :: a -> Doc (Ann a)
  default prettyText :: Show a => a -> Doc (Ann a)
  prettyText = text . Show.show
