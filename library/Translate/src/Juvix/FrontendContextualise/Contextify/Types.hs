{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.FrontendContextualise.Contextify.Types where

import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Desugar.Types as Repr
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

type Repr f =
  f (NonEmpty (Repr.FunctionLike Repr.Expression)) Repr.Signature Repr.Type

type Context =
  Repr Context.T

type Definition =
  Repr Context.Definition

data Pass
  = P
      { ctx :: Context,
        opens :: [NameSymbol.T],
        modsDefined :: [NameSymbol.T]
      }
  deriving (Show)
