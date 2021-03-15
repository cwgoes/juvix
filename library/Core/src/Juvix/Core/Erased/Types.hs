module Juvix.Core.Erased.Types
  ( module Juvix.Core.Erased.Types,
    Term' (..),
    Type' (..),
    TypeAssignment',
  )
where

-- TODO shouldn't this module be using these?
-- import Juvix.Core.Erased.Extend
import Juvix.Core.Erased.Types.Base
import qualified Juvix.Core.IR.Typechecker.Types as TC
import Juvix.Core.IR.Types.Base hiding
  ( Term' (..),
    defaultExtTerm,
    extendTerm,
  )
import Juvix.Core.IR.Types.Globals
import Juvix.Library hiding (Datatype, Type)

data T

extendTerm "Term" [] [t|T|] (\_ -> defaultExtTerm)

type TermT primTy primVal = Term (TC.TypedPrim primTy primVal)

extendType "Type" [] [t|T|] (\_ -> defaultExtType)

type Datatype = Datatype' T T

type DataArg = DataArg' T

type DataCon = DataCon' T T

type Function = Function' T

type FunClause primTy primVal = FunClause' T primTy primVal

type TypeAssignment primTy = TypeAssignment' T primTy

data EvaluationError primVal
  = PrimitiveApplicationError primVal primVal
  deriving (Show, Eq, Generic)
