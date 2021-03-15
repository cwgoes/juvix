{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.IR.Types.Globals where

import Data.Kind (Constraint)
import qualified Data.Map as Map
import Juvix.Core.IR.Types.Base
import Juvix.Library hiding (Pos)
import Juvix.Library.HashMap (HashMap)
import Juvix.Library.Usage (Usage)

type RawGlobalAll (c :: * -> Constraint) ext primTy primVal =
  ( c primTy,
    c primVal,
    TermAll c ext primTy primVal,
    ElimAll c ext primTy primVal,
    PatternAll c ext primTy primVal
  )

type GlobalAllV (c :: * -> Constraint) ext primTy primVal =
  ( c primTy,
    c primVal,
    ValueAll c ext primTy primVal,
    NeutralAll c ext primTy primVal
  )

type GlobalAll (c :: * -> Constraint) extV extT primTy primVal =
  ( c primTy,
    c primVal,
    TermAll c extT primTy primVal,
    ElimAll c extT primTy primVal,
    ValueAll c extV primTy primVal,
    NeutralAll c extV primTy primVal,
    PatternAll c extT primTy primVal
  )

data RawDatatype' ext primTy primVal
  = RawDatatype
      { rawDataName :: GlobalName,
        -- | the positivity of its parameters
        rawDataPos :: [Pos],
        -- | the type constructor's arguments
        rawDataArgs :: [RawDataArg' ext primTy primVal],
        -- | the type constructor's target universe level
        rawDataLevel :: Natural,
        rawDataCons :: [RawDataCon' ext primTy primVal]
      }
  deriving (Generic)

deriving instance
  RawGlobalAll Show ext primTy primVal =>
  Show (RawDatatype' ext primTy primVal)

deriving instance
  RawGlobalAll Eq ext primTy primVal =>
  Eq (RawDatatype' ext primTy primVal)

deriving instance
  (Data ext, RawGlobalAll Data ext primTy primVal) =>
  Data (RawDatatype' ext primTy primVal)

deriving instance
  RawGlobalAll NFData ext primTy primVal =>
  NFData (RawDatatype' ext primTy primVal)

data Datatype' extV extT primTy primVal
  = Datatype
      { dataName :: GlobalName,
        -- | the positivity of its parameters
        dataPos :: [Pos],
        -- | type checked arguments
        dataArgs :: [DataArg' extV primTy primVal],
        -- | the type constructor's target universe level
        dataLevel :: Natural,
        dataCons :: [DataCon' extV extT primTy primVal]
      }
  deriving (Generic)

deriving instance
  GlobalAll Show extV extT primTy primVal =>
  Show (Datatype' extV extT primTy primVal)

deriving instance
  GlobalAll Eq extV extT primTy primVal =>
  Eq (Datatype' extV extT primTy primVal)

deriving instance
  (Data extV, Data extT, GlobalAll Data extV extT primTy primVal) =>
  Data (Datatype' extV extT primTy primVal)

deriving instance
  GlobalAll NFData extV extT primTy primVal =>
  NFData (Datatype' extV extT primTy primVal)

data RawDataArg' ext primTy primVal
  = RawDataArg
      { rawArgName :: GlobalName,
        rawArgUsage :: Usage,
        rawArgType :: Term' ext primTy primVal
      }
  deriving (Generic)

deriving instance
  RawGlobalAll Show ext primTy primVal =>
  Show (RawDataArg' ext primTy primVal)

deriving instance
  RawGlobalAll Eq ext primTy primVal =>
  Eq (RawDataArg' ext primTy primVal)

deriving instance
  (Data ext, RawGlobalAll Data ext primTy primVal) =>
  Data (RawDataArg' ext primTy primVal)

deriving instance
  RawGlobalAll NFData ext primTy primVal =>
  NFData (RawDataArg' ext primTy primVal)

data DataArg' ext primTy primVal
  = DataArg
      { argName :: GlobalName,
        argUsage :: Usage,
        argType :: Value' ext primTy primVal
      }
  deriving (Generic)

deriving instance
  GlobalAllV Show ext primTy primVal =>
  Show (DataArg' ext primTy primVal)

deriving instance
  GlobalAllV Eq ext primTy primVal =>
  Eq (DataArg' ext primTy primVal)

deriving instance
  (Data ext, GlobalAllV Data ext primTy primVal) =>
  Data (DataArg' ext primTy primVal)

deriving instance
  GlobalAllV NFData ext primTy primVal =>
  NFData (DataArg' ext primTy primVal)

data RawDataCon' ext primTy primVal
  = RawDataCon
      { rawConName :: GlobalName,
        rawConType :: Term' ext primTy primVal,
        rawConDef :: Maybe (RawFunction' ext primTy primVal)
      }
  deriving (Generic)

deriving instance
  RawGlobalAll Show ext primTy primVal =>
  Show (RawDataCon' ext primTy primVal)

deriving instance
  RawGlobalAll Eq ext primTy primVal =>
  Eq (RawDataCon' ext primTy primVal)

deriving instance
  (Data ext, RawGlobalAll Data ext primTy primVal) =>
  Data (RawDataCon' ext primTy primVal)

deriving instance
  RawGlobalAll NFData ext primTy primVal =>
  NFData (RawDataCon' ext primTy primVal)

data DataCon' extV extT primTy primVal
  = DataCon
      { conName :: GlobalName,
        conType :: Value' extV primTy primVal,
        conDef :: Maybe (Function' extV extT primTy primVal)
      }
  deriving (Generic)

deriving instance
  GlobalAll Show extV extT primTy primVal =>
  Show (DataCon' extV extT primTy primVal)

deriving instance
  GlobalAll Eq extV extT primTy primVal =>
  Eq (DataCon' extV extT primTy primVal)

deriving instance
  (Data extV, Data extT, GlobalAll Data extV extT primTy primVal) =>
  Data (DataCon' extV extT primTy primVal)

deriving instance
  GlobalAll NFData extV extT primTy primVal =>
  NFData (DataCon' extV extT primTy primVal)

data RawFunction' ext primTy primVal
  = RawFunction
      { rawFunName :: GlobalName,
        rawFunUsage :: GlobalUsage,
        rawFunType :: Term' ext primTy primVal,
        rawFunClauses :: NonEmpty (RawFunClause' ext primTy primVal)
      }
  deriving (Generic)

deriving instance
  RawGlobalAll Show ext primTy primVal =>
  Show (RawFunction' ext primTy primVal)

deriving instance
  RawGlobalAll Eq ext primTy primVal =>
  Eq (RawFunction' ext primTy primVal)

deriving instance
  (Data ext, RawGlobalAll Data ext primTy primVal) =>
  Data (RawFunction' ext primTy primVal)

deriving instance
  RawGlobalAll NFData ext primTy primVal =>
  NFData (RawFunction' ext primTy primVal)

data Function' extV extT primTy primVal
  = Function
      { funName :: GlobalName,
        funUsage :: GlobalUsage,
        funType :: Value' extV primTy primVal,
        funClauses :: NonEmpty (FunClause' extV extT primTy primVal)
      }
  deriving (Generic)

deriving instance
  GlobalAll Show extV extT primTy primVal =>
  Show (Function' extV extT primTy primVal)

deriving instance
  GlobalAll Eq extV extT primTy primVal =>
  Eq (Function' extV extT primTy primVal)

deriving instance
  (Data extV, Data extT, GlobalAll Data extV extT primTy primVal) =>
  Data (Function' extV extT primTy primVal)

deriving instance
  GlobalAll NFData extV extT primTy primVal =>
  NFData (Function' extV extT primTy primVal)

deriving instance
  GlobalAll Show extV extT primTy primVal =>
  Show (FunClause' extV extT primTy primVal)

deriving instance
  GlobalAll Eq extV extT primTy primVal =>
  Eq (FunClause' extV extT primTy primVal)

deriving instance
  ( Data extV,
    Data extT,
    GlobalAll Data extV extT primTy primVal
  ) =>
  Data (FunClause' extV extT primTy primVal)

deriving instance
  GlobalAll NFData extV extT primTy primVal =>
  NFData (FunClause' extV extT primTy primVal)

data RawAbstract' ext primTy primVal
  = RawAbstract
      { rawAbsName :: GlobalName,
        rawAbsUsage :: GlobalUsage,
        rawAbsType :: Term' ext primTy primVal
      }
  deriving (Generic)

deriving instance
  RawGlobalAll Show ext primTy primVal =>
  Show (RawAbstract' ext primTy primVal)

deriving instance
  RawGlobalAll Eq ext primTy primVal =>
  Eq (RawAbstract' ext primTy primVal)

deriving instance
  (Data ext, RawGlobalAll Data ext primTy primVal) =>
  Data (RawAbstract' ext primTy primVal)

deriving instance
  RawGlobalAll NFData ext primTy primVal =>
  NFData (RawAbstract' ext primTy primVal)

data Abstract' ext primTy primVal
  = Abstract
      { absName :: GlobalName,
        absUsage :: GlobalUsage,
        absType :: Value' ext primTy primVal
      }
  deriving (Generic)

deriving instance
  GlobalAllV Show ext primTy primVal =>
  Show (Abstract' ext primTy primVal)

deriving instance
  GlobalAllV Eq ext primTy primVal =>
  Eq (Abstract' ext primTy primVal)

deriving instance
  (Data ext, GlobalAllV Data ext primTy primVal) =>
  Data (Abstract' ext primTy primVal)

deriving instance
  GlobalAllV NFData ext primTy primVal =>
  NFData (Abstract' ext primTy primVal)

data RawGlobal' ext primTy primVal
  = RawGDatatype (RawDatatype' ext primTy primVal)
  | RawGDataCon (RawDataCon' ext primTy primVal)
  | RawGFunction (RawFunction' ext primTy primVal)
  | RawGAbstract (RawAbstract' ext primTy primVal)
  deriving (Generic)

deriving instance
  RawGlobalAll Show ext primTy primVal =>
  Show (RawGlobal' ext primTy primVal)

deriving instance
  RawGlobalAll Eq ext primTy primVal =>
  Eq (RawGlobal' ext primTy primVal)

deriving instance
  (Data ext, RawGlobalAll Data ext primTy primVal) =>
  Data (RawGlobal' ext primTy primVal)

deriving instance
  RawGlobalAll NFData ext primTy primVal =>
  NFData (RawGlobal' ext primTy primVal)

data Global' extV extT primTy primVal
  = GDatatype (Datatype' extV extT primTy primVal)
  | GDataCon (DataCon' extV extT primTy primVal)
  | GFunction (Function' extV extT primTy primVal)
  | GAbstract (Abstract' extV primTy primVal)
  deriving (Generic)

deriving instance
  GlobalAll Show extV extT primTy primVal =>
  Show (Global' extV extT primTy primVal)

deriving instance
  GlobalAll Eq extV extT primTy primVal =>
  Eq (Global' extV extT primTy primVal)

deriving instance
  (Data extV, Data extT, GlobalAll Data extV extT primTy primVal) =>
  Data (Global' extV extT primTy primVal)

deriving instance
  GlobalAll NFData extV extT primTy primVal =>
  NFData (Global' extV extT primTy primVal)

type RawGlobals' ext primTy primVal =
  HashMap GlobalName (RawGlobal' ext primTy primVal)

type Globals' extV extT primTy primVal =
  HashMap GlobalName (Global' extV extT primTy primVal)

type RawTelescope ext primTy primVal =
  [(Name, Term' ext primTy primVal)]

type Telescope ext primTy primVal =
  [(Name, Value' ext primTy primVal)]

data FunClause' extV extT primTy primVal
  = FunClause
      { -- | @Δ@: The types of the pattern variables in dependency order.
        -- , namedClausePats :: NAPs (Using Name instead atm)
        -- ^ @Δ ⊢ ps@.  The de Bruijn indices refer to @Δ@.
        clauseTel :: Telescope extV primTy primVal,
        clausePats :: [Pattern' extT primTy primVal], --TODO [SplitPattern]
            -- TODO make it a Maybe
            -- @Just v@ for a regular clause, @Nothing@ for an absurd one.
        clauseBody :: Term' extT primTy primVal,
        -- | @Δ ⊢ t@.  The type of the rhs under @clauseTel@.
        clauseType :: Maybe (Value' extV primTy primVal),
        -- \| @clauseBody@ contains recursive calls; computed by termination
        -- checker.
        --   @Nothing@ means that termination checker has not run yet,
        --   or that @clauseBody@ contains meta-variables;
        --   these could be filled with recursive calls later!
        --   @Just False@ means definitely no recursive call.
        --   @Just True@ means definitely a recursive call.
        -- TODO add this when termination checking
        -- clauseRecursive   :: Maybe Bool,

        -- | Clause has been labelled as CATCHALL.
        clauseCatchall :: Bool,
        -- | Clause has been labelled as unreachable by the coverage checker.
        --   @Nothing@ means coverage checker has not run yet (clause may be unreachable).
        --   @Just False@ means clause is not unreachable.
        --   @Just True@ means clause is unreachable.
        clauseUnreachable :: Maybe Bool
      }
  deriving (Generic)

-- | see 'FunClause'' for description of fields.
data RawFunClause' ext primTy primVal
  = RawFunClause
      { rawClauseTel :: RawTelescope ext primTy primVal,
        rawClausePats :: [Pattern' ext primTy primVal], --TODO [SplitPattern]
        rawClauseBody :: Term' ext primTy primVal,
        rawClauseCatchall :: Bool
      }
  deriving (Generic)

deriving instance
  RawGlobalAll Show ext primTy primVal =>
  Show (RawFunClause' ext primTy primVal)

deriving instance
  RawGlobalAll Eq ext primTy primVal =>
  Eq (RawFunClause' ext primTy primVal)

deriving instance
  (Data ext, RawGlobalAll Data ext primTy primVal) =>
  Data (RawFunClause' ext primTy primVal)

deriving instance
  RawGlobalAll NFData ext primTy primVal =>
  NFData (RawFunClause' ext primTy primVal)

type Signature ty ext primTy primVal = Map.Map GlobalName (SigDef ty ext primTy primVal)

-- Return type of all type-checking functions.
-- state monad for global signature
-- TODO move this somewhere
type TypeCheck ty ext primTy primVal a =
  StateT (Signature ty ext primTy primVal) IO a

data SigDef extV extT primTy primVal
  = -- | function constant to its type, clauses
    FunSig
      (Value' extV primTy primVal)
      ( Either
          (NonEmpty (RawFunClause' extT primTy primVal))
          (NonEmpty (FunClause' extV extT primTy primVal))
      )
  | -- | constructor constant to its type
    ConSig (Value' extV primTy primVal)
  | -- | data type constant to # parameters, positivity of parameters, type
    DataSig Int [Pos] (Value' extV primTy primVal)

-- | Positivity
data Pos
  = -- | strictly positive
    SPos
  | -- | other
    NSPos
  deriving (Generic, Eq, Show, Data, NFData)
