{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.Erasure.Types
  ( module Juvix.Core.Erasure.Types,
    module Type,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Extensible as Ext
import Juvix.Core.Erased.Types as Type
  ( Type,
    pattern Pi,
    pattern PrimTy,
    pattern Sig,
    pattern Star,
    pattern SymT,
    pattern UnitTy,
  )
import qualified Juvix.Core.Erased.Types as Erased
import qualified Juvix.Core.Erased.Types.Base as Erased
import qualified Juvix.Core.IR.Typechecker.Types as Typed
import Juvix.Core.IR.Types (GlobalName, GlobalUsage, PatternVar)
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library hiding (Datatype, Type, empty)
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Usage (Usage)

type MapPrim p1 p2 ty val =
  [NameSymbol.T] -> p1 -> Either (Error ty val) p2

data Env primTy1 primTy2 primVal1 primVal2
  = Env
      { nextName :: Int,
        nameStack :: [NameSymbol.T],
        mapPrimTy :: MapPrim primTy1 primTy2 primTy1 primVal1,
        mapPrimVal :: MapPrim primVal1 primVal2 primTy1 primVal1
      }
  deriving (Generic)

type EnvEraAlias primTy1 primTy2 primVal1 primVal2 =
  ExceptT
    (Error primTy1 primVal1)
    (State (Env primTy1 primTy2 primVal1 primVal2))

newtype EnvT primTy1 primTy2 primVal1 primVal2 a
  = EnvEra (EnvEraAlias primTy1 primTy2 primVal1 primVal2 a)
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "nextName" Int,
      HasSink "nextName" Int,
      HasSource "nextName" Int
    )
    via StateField "nextName" (EnvEraAlias primTy1 primTy2 primVal1 primVal2)
  deriving
    ( HasState "nameStack" [NameSymbol.T],
      HasSink "nameStack" [NameSymbol.T],
      HasSource "nameStack" [NameSymbol.T]
    )
    via StateField "nameStack" (EnvEraAlias primTy1 primTy2 primVal1 primVal2)
  deriving
    ( HasSource "mapPrimTy" (MapPrim primTy1 primTy2 primTy1 primVal1),
      HasReader "mapPrimTy" (MapPrim primTy1 primTy2 primTy1 primVal1)
    )
    via ReaderField "mapPrimTy" (EnvEraAlias primTy1 primTy2 primVal1 primVal2)
  deriving
    ( HasSource "mapPrimVal" (MapPrim primVal1 primVal2 primTy1 primVal1),
      HasReader "mapPrimVal" (MapPrim primVal1 primVal2 primTy1 primVal1)
    )
    via ReaderField "mapPrimVal" (EnvEraAlias primTy1 primTy2 primVal1 primVal2)
  deriving
    (HasThrow "erasureError" (Error primTy1 primVal1))
    via MonadError (EnvEraAlias primTy1 primTy2 primVal1 primVal2)

exec ::
  MapPrim primTy1 primTy2 primTy1 primVal1 ->
  MapPrim primVal1 primVal2 primTy1 primVal1 ->
  EnvT primTy1 primTy2 primVal1 primVal2 a ->
  Either (Error primTy1 primVal1) a
exec mt mv (EnvEra m) = evalState (runExceptT m) (Env 0 [] mt mv)

data Error primTy primVal
  = UnsupportedTermT (Typed.Term' primTy primVal)
  | UnsupportedTermE (Typed.Elim' primTy primVal)
  | UnsupportedTypeV (IR.Value primTy primVal)
  | UnsupportedTypeN (IR.Neutral primTy primVal)
  | CannotEraseZeroUsageTerm (Typed.Term' primTy primVal)
  | InternalError Text
  deriving (Generic)

deriving instance
  ( Show primTy,
    Show primVal,
    Show (Param.ApplyErrorExtra primTy)
  ) =>
  Show (Error primTy primVal)

deriving instance
  ( Eq primTy,
    Eq primVal,
    Eq (Param.ApplyErrorExtra primTy)
  ) =>
  Eq (Error primTy primVal)

data T primTy

do
  primTy' <- Ext.newName "primTy"
  let primTy = Ext.varT primTy'
  let typed = Just [[t|Type $primTy|]]
  let typedTuple = Just [[t|(Type $primTy, Type $primTy)|]]
  Erased.extendTerm "Term" [primTy'] [t|T $primTy|] $
    \_ ->
      Erased.defaultExtTerm
        { Erased.typeVar = typed,
          Erased.typePrim = typed,
          Erased.typeLam = typed,
          Erased.typePair = typed,
          Erased.typeUnit = typed,
          Erased.typeLet = typedTuple,
          Erased.typeApp = typed
        }

type TermT primTy primVal = Term primTy (Typed.TypedPrim primTy primVal)

-- TODO: Figure out how to do this with extensible.
-- IR.extendDatatype "Datatype" [] [t|T|] extDatatype

data Datatype primTy primVal
  = Datatype
      { dataName :: GlobalName,
        dataArgs :: [DataArg primTy],
        dataLevel :: Natural,
        dataCons :: [DataCon primTy primVal]
      }

-- TODO: Figure out how to do this with extensible.
-- IR.extendDataArg "DataArg" [] [t|T|] extDataArg

data DataArg primTy
  = DataArg
      { argName :: GlobalName,
        argUsage :: Usage,
        argType :: Type primTy
      }

-- TODO: Figure out how to do this with extensible.
-- IR.extendDataCon "DataCon" [] [t|T|] extDataCon

data DataCon primTy primVal
  = DataCon
      { conName :: GlobalName,
        conType :: Type primTy,
        conDef :: Maybe (Function primTy primVal)
      }

-- TODO: Figure out how to do this with extensible.
-- IR.extendFunction "Function" [] [t|T|] extFunction

data Function primTy primVal
  = Function
      { funName :: GlobalName,
        funUsage :: GlobalUsage,
        funType :: Type primTy,
        funClauses :: NonEmpty (FunClause primTy primVal)
      }

type FunctionT primTy primVal =
  Function primTy (Typed.TypedPrim primTy primVal)

-- TODO: Figure out how to do this with extensible.
-- IR.extendFunClause "FunClause" [] [t|T|] extFunClause

data FunClause primTy primVal
  = FunClause [Pattern primTy primVal] (Term primTy primVal)

type FunClauseT primTy primVal =
  FunClause primTy (Typed.TypedPrim primTy primVal)

-- TODO: Figure out how to do this with extensible.
-- IR.extendPattern "Pattern" [] [t|T|] extPattern

data Pattern primTy primVal
  = PCon GlobalName [Pattern primTy primVal]
  | PPair (Pattern primTy primVal) (Pattern primTy primVal)
  | PUnit
  | PVar PatternVar
  | PDot (Term primTy primVal)
  | PPrim primVal

type PatternT primTy primVal =
  Pattern primTy (Typed.TypedPrim primTy primVal)

data Abstract primTy
  = Abstract
      { absName :: GlobalName,
        absUsage :: GlobalUsage,
        absType :: Type primTy
      }

data Global primTy primVal
  = GDatatype (Datatype primTy primVal)
  | GDataCon (DataCon primTy primVal)
  | GFunction (Function primTy primVal)
  | GAbstract (Abstract primTy)

type Globals primTy primVal = HM.HashMap GlobalName (Global primTy primVal)

type GlobalT primTy primVal = Global primTy (Typed.TypedPrim primTy primVal)

type GlobalsT primTy primVal = Globals primTy (Typed.TypedPrim primTy primVal)

getType :: Term primTy primVal -> Type primTy
getType (Var _ ty) = ty
getType (Prim _ ty) = ty
getType (Lam _ _ ty) = ty
getType (Pair _ _ ty) = ty
getType (Unit ty) = ty
getType (Let _ _ _ (_, ty)) = ty
getType (App _ _ ty) = ty

eraseAnn :: Term primTy primVal -> Erased.Term primVal
eraseAnn (Var sym _) = Erased.Var sym
eraseAnn (Prim p _) = Erased.Prim p
eraseAnn (Lam s b _) = Erased.Lam s (eraseAnn b)
eraseAnn (Pair a b _) = Erased.Pair (eraseAnn a) (eraseAnn b)
eraseAnn (Unit _) = Erased.Unit
eraseAnn (Let s a b _) = Erased.Let s (eraseAnn a) (eraseAnn b)
eraseAnn (App a b _) = Erased.App (eraseAnn a) (eraseAnn b)
