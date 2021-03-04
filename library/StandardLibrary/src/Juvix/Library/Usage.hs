{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Juvix.Library.Usage
  ( Usage,
    NatAndw (..),
    T,
    numToNat,
    allowsUsageOf,
    allows,
    pred,
    minus,
  )
where

import Juvix.Library hiding (pred, show)
import qualified Juvix.Library.PrettyPrint as Pp

-- | Usage is an alias for the semiring representation
type T = NatAndw

type Usage = NatAndw

-- | NatAndw is the choice of the semiring for ({ℕ, ω}, (+), 0, (*), 1)
data NatAndw
  = -- | semiring of (Nat,w) for usage annotation
    -- 0, 1, or n usage
    SNat Natural
  | -- | unspecified usage
    Omega
  deriving (Eq, Show, Read, Generic, Data, NFData)

-- Addition is the semi-Ring/Monoid instance
instance Semigroup NatAndw where
  SNat x <> SNat y = SNat (x + y)
  Omega <> _ = Omega
  _ <> Omega = Omega

instance Monoid NatAndw where
  mempty = SNat 0

-- Semiring instance is thus multiplication
instance Semiring NatAndw where
  one = SNat 1

  SNat x <.> SNat y = SNat (x * y)
  Omega <.> _ = Omega
  _ <.> Omega = Omega

type instance Pp.PrettyAnn NatAndw = ()

instance Pp.PrettySyntax NatAndw where
  prettyPrec' (SNat π) = pure $ Pp.show π
  prettyPrec' Omega    = pure "ω"

pred :: NatAndw -> NatAndw
pred (SNat x) = SNat (x - 1)
pred Omega = Omega

minus :: Usage -> Usage -> Maybe Usage
minus Omega _ = Just Omega
minus (SNat i) (SNat j) | i >= j = Just $ SNat $ i - j
minus _ _ = Nothing

infixl 6 `minus` -- same as -

-- | numToNat is a helper function that converts an integer to NatAndW
numToNat :: Integer -> NatAndw
numToNat = SNat . fromInteger

-- variables annotated with n can be used n times.
-- variables annotated with Omega can be used any times.

-- | allowsUsageOf is the function that checks usage compatibility
allowsUsageOf :: Usage -> Usage -> Bool
allowsUsageOf (SNat x) (SNat y) = x == y
allowsUsageOf Omega (SNat _) = True
allowsUsageOf Omega Omega = True
allowsUsageOf (SNat _) Omega = False

allows :: Usage -> Usage -> Bool
allows = allowsUsageOf

infix 4 `allowsUsageOf`, `allows` -- same as <=
