{-# LANGUAGE AllowAmbiguousTypes #-}

module Juvix.Library.PrettyPrint
  ( module Juvix.Library.PrettyPrint,
    module Text.PrettyPrint.Compact
  ) where

import Text.PrettyPrint.Compact
import Juvix.Library


-- | Annotations, which can be used for e.g. syntax highlighting
type family PrettyAnn a :: *

type Prec = Natural

-- | Class for pretty-printing syntax-like types, which need to keep track of
-- the surrounding precedence level.
class Monoid (PrettyAnn a) => PrettySyntax a where
  -- | Pretty-prints a syntax value, given the current precedence value in
  -- a reader environment.
  prettyPrec' ::
    HasReader "prec" Prec m =>
    a -> m (Doc (PrettyAnn a))
  default prettyPrec' ::
    (PrettyText a, HasReader "prec" Prec m) =>
    a -> m (Doc (PrettyAnn a))
  prettyPrec' = pure . prettyText

-- | Pretty-print at the given precedence level.
prettyPrec :: PrettySyntax a => Prec -> a -> Doc (PrettyAnn a)
prettyPrec prec x =
  let MonadReader act = prettyPrec' x in
  runReader act prec

-- | Pretty-print at the initial precedence level.
prettyPrec0 :: PrettySyntax a => a -> Doc (PrettyAnn a)
prettyPrec0 = prettyPrec 0

-- | Class for text-like types (e.g. messages), which don't have a concept of
-- precedence.
class Monoid (PrettyAnn a) => PrettyText a where
  -- | Pretty-print a value as human-readable text.
  prettyText :: a -> Doc (PrettyAnn a)
  default prettyText :: Show a => a -> Doc (PrettyAnn a)
  prettyText = text . show
