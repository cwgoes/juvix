{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.FrontendContextualise.Contextify.Types where

import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Desugar.Types as Repr
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp

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

-- Temporary while we have both around

type ContextSexp =
  Context.T Sexp.T Sexp.T Sexp.T

type DefinitionSexp =
  Context.Definition Sexp.T Sexp.T Sexp.T

data PassSexp
  = PS
      { ctxS :: ContextSexp,
        opensS :: [NameSymbol.T],
        modsDefinedS :: [NameSymbol.T]
      }
  deriving (Show)
