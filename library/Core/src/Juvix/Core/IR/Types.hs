-- | Quantitative type implementation inspired by
--   Atkey 2018 and McBride 2016.
module Juvix.Core.IR.Types
  ( module Juvix.Core.IR.Types,
    Name (..),
    GlobalUsage (..),
    GlobalName,
    PatternVar,
    BoundVar,
    Universe,
    RawDatatype' (..),
    Datatype' (..),
    RawDataArg' (..),
    DataArg' (..),
    RawDataCon' (..),
    DataCon' (..),
    RawFunction' (..),
    Function' (..),
    RawFunClause' (..),
    FunClause' (..),
    RawAbstract' (..),
    Abstract' (..),
    RawGlobal' (..),
    Global' (..),
    RawGlobals',
    Globals',
  )
where

import Juvix.Core.IR.Types.Base
import Juvix.Core.IR.Types.Globals
import Juvix.Library hiding (show)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

data NoExt deriving (Data)

extendTerm "Term" [] [t|NoExt|] $ \_ _ -> defaultExtTerm

extendElim "Elim" [] [t|NoExt|] $ \_ _ -> defaultExtElim

extendValue "Value" [] [t|NoExt|] $ \_ _ -> defaultExtValue

extendNeutral "Neutral" [] [t|NoExt|] $ \_ _ -> defaultExtNeutral

extendPattern "Pattern" [] [t|NoExt|] $ \_ _ -> defaultExtPattern

type Datatype = Datatype' NoExt NoExt

type RawDatatype = RawDatatype' NoExt

type DataArg = DataArg' NoExt

type RawDataArg = RawDataArg' NoExt

type DataCon = DataCon' NoExt NoExt

type RawDataCon = RawDataCon' NoExt

type Function = Function' NoExt NoExt

type RawFunction = RawFunction' NoExt

type FunClause = FunClause' NoExt NoExt

type RawFunClause = RawFunClause' NoExt

type Abstract = Abstract' NoExt

type RawAbstract = RawAbstract' NoExt

type Global = Global' NoExt NoExt

type RawGlobal = RawGlobal' NoExt

type Globals primTy primVal = Globals' NoExt NoExt primTy primVal

type RawGlobals primTy primVal = RawGlobals' NoExt primTy primVal

-- Quotation: takes a value back to a term
quote0 :: Value primTy primVal -> Term primTy primVal
quote0 = quote
{-# DEPRECATED quote0 "use quote directly" #-}

quote :: Value primTy primVal -> Term primTy primVal
quote (VStar nat) = Star nat
quote (VPrimTy p) = PrimTy p
quote (VPi π s t) = Pi π (quote s) (quote t)
quote (VLam s) = Lam (quote s)
quote (VSig π s t) = Sig π (quote s) (quote t)
quote (VPair s t) = Pair (quote s) (quote t)
quote VUnitTy = UnitTy
quote VUnit = Unit
quote (VPrim pri) = Prim pri
quote (VNeutral n) = Elim $ neutralQuote n

neutralQuote :: Neutral primTy primVal -> Elim primTy primVal
neutralQuote (NBound x) = Bound x
neutralQuote (NFree x) = Free x
neutralQuote (NApp n v) = App (neutralQuote n) (quote v)

-- | 'VFree' creates the value corresponding to a free variable
pattern VFree ::
  ( XNFree ext primTy primVal ~ (),
    XVNeutral ext primTy primVal ~ ()
  ) =>
  Name ->
  Value' ext primTy primVal
pattern VFree n = VNeutral' (NFree' n ()) ()

-- | 'VBound' creates the value corresponding to a bound variable
pattern VBound ::
  ( XNBound ext primTy primVal ~ (),
    XVNeutral ext primTy primVal ~ ()
  ) =>
  BoundVar ->
  Value' ext primTy primVal
pattern VBound n = VNeutral' (NBound' n ()) ()

usageToGlobal :: Usage.T -> Maybe GlobalUsage
usageToGlobal Usage.Omega = Just GOmega
usageToGlobal (Usage.SNat 0) = Just GZero
usageToGlobal _ = Nothing

globalToUsage :: GlobalUsage -> Usage.T
globalToUsage GOmega = Usage.Omega
globalToUsage GZero = Usage.SNat 0

globalName :: Global' extT extV primTy primVal -> NameSymbol.T
globalName (GDatatype (Datatype {dataName})) = dataName
globalName (GDataCon (DataCon {conName})) = conName
globalName (GFunction (Function {funName})) = funName
globalName (GAbstract (Abstract {absName})) = absName
