{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.IR.Evaluator.Types where

import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library

data ApplyError primTy primVal
  = NoApplyError
  | ApplyErrorV (Param.ApplyError primVal)
  | ApplyErrorT (Param.ApplyError primTy)

deriving instance
  ( Eq primTy,
    Eq primVal,
    Eq (Param.Arg primTy),
    Eq (Param.Arg primVal),
    Eq (Param.ApplyErrorExtra primTy),
    Eq (Param.ApplyErrorExtra primVal)
  ) =>
  Eq (ApplyError primTy primVal)

deriving instance
  ( Show primTy,
    Show primVal,
    Show (Param.Arg primTy),
    Show (Param.Arg primVal),
    Show (Param.ApplyErrorExtra primTy),
    Show (Param.ApplyErrorExtra primVal)
  ) =>
  Show (ApplyError primTy primVal)

data Error extV extT primTy primVal
  = CannotApply
      { fun, arg :: IR.Value' extV primTy primVal,
        paramErr :: ApplyError primTy primVal
      }
  | UnsupportedTermExt (IR.TermX extT primTy primVal)
  | UnsupportedElimExt (IR.ElimX extT primTy primVal)

deriving instance
  ( Eq primTy,
    Eq primVal,
    IR.ValueAll Eq extV primTy primVal,
    IR.NeutralAll Eq extV primTy primVal,
    Eq (Param.Arg primTy),
    Eq (Param.Arg primVal),
    Eq (Param.ApplyErrorExtra primTy),
    Eq (Param.ApplyErrorExtra primVal),
    Eq (IR.TermX extT primTy primVal),
    Eq (IR.ElimX extT primTy primVal)
  ) =>
  Eq (Error extV extT primTy primVal)

deriving instance
  ( Show primTy,
    Show primVal,
    IR.ValueAll Show extV primTy primVal,
    IR.NeutralAll Show extV primTy primVal,
    Show (Param.Arg primTy),
    Show (Param.Arg primVal),
    Show (Param.ApplyErrorExtra primTy),
    Show (Param.ApplyErrorExtra primVal),
    Show (IR.TermX extT primTy primVal),
    Show (IR.ElimX extT primTy primVal)
  ) =>
  Show (Error extV extT primTy primVal)

type TermExtFun extG extT primTy primVal =
  LookupFun extG primTy primVal ->
  IR.TermX extT primTy primVal ->
  Either (Error IR.NoExt extT primTy primVal) (IR.Value primTy primVal)

type ElimExtFun extG extT primTy primVal =
  LookupFun extG primTy primVal ->
  IR.ElimX extT primTy primVal ->
  Either (Error IR.NoExt extT primTy primVal) (IR.Value primTy primVal)

data ExtFuns extG extT primTy primVal
  = ExtFuns
      { tExtFun :: TermExtFun extG extT primTy primVal,
        eExtFun :: ElimExtFun extG extT primTy primVal
      }

rejectExts :: ExtFuns extG extT primTy primVal
rejectExts =
  ExtFuns
    { tExtFun = \_ -> Left . UnsupportedTermExt,
      eExtFun = \_ -> Left . UnsupportedElimExt
    }

type LookupFun ext primTy primVal =
  IR.GlobalName -> Maybe (IR.Elim' ext primTy primVal)
