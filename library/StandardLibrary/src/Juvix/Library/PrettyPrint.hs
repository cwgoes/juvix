{-# LANGUAGE AllowAmbiguousTypes #-}

module Juvix.Library.PrettyPrint
  ( module Juvix.Library.PrettyPrint,
    module Text.PrettyPrint.Compact
  ) where

import Text.PrettyPrint.Compact
import Juvix.Library


type family PrettyAnn a :: *

-- | Class for pretty-printing syntax-like types, which need to keep track of
-- the surrounding precedence level.
class Monoid (PrettyAnn a) => PrettySyntax a where
  -- | The type of precedence levels. Defaults to 'Natural'.
  type PrettyPrec a
  type PrettyPrec a = Natural

  -- | The initial precedence level. Defaults to @0@.
  initPrec :: PrettyPrec a
  default initPrec :: Num (PrettyPrec a) => PrettyPrec a
  initPrec = 0

  -- | Pretty-prints a syntax value, given the current precedence value in
  -- a reader environment.
  prettyPrec' ::
    HasReader "prec" (PrettyPrec a) m =>
    a -> m (Doc (PrettyAnn a))
  default prettyPrec' ::
    (PrettyText a, HasReader "prec" (PrettyPrec a) m) =>
    a -> m (Doc (PrettyAnn a))
  prettyPrec' = pure . prettyText

-- | Pretty-print at the given precedence level.
prettyPrec :: PrettySyntax a => PrettyPrec a -> a -> Doc (PrettyAnn a)
prettyPrec prec x =
  let MonadReader act = prettyPrec' x in
  runReader act prec

-- | Pretty-print at the initial precedence level.
prettyPrec0 :: forall a. PrettySyntax a => a -> Doc (PrettyAnn a)
prettyPrec0 = prettyPrec (initPrec @a)

-- | Class for text-like types (e.g. messages), which don't have a concept of
-- precedence.
class Monoid (PrettyAnn a) => PrettyText a where
  -- | Pretty-print a value as human-readable text.
  prettyText :: a -> Doc (PrettyAnn a)
  default prettyText :: Show a => a -> Doc (PrettyAnn a)
  prettyText = text . show
