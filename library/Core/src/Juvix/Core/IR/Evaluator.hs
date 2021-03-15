{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- This includes the evaluators (evalTerm and evalElim),
-- the value application function (vapp) and
-- the substitution functions (substTerm and substElim).
module Juvix.Core.IR.Evaluator
  ( module Juvix.Core.IR.Evaluator,
    module Juvix.Core.IR.Evaluator.Types,
    module Juvix.Core.IR.Evaluator.Weak,
    module Juvix.Core.IR.Evaluator.Subst,
    module Juvix.Core.IR.Evaluator.SubstV,
    module Juvix.Core.IR.Evaluator.PatSubst,
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap as IntMap
import Juvix.Core.IR.Evaluator.PatSubst
import Juvix.Core.IR.Evaluator.Subst
import Juvix.Core.IR.Evaluator.SubstV
import Juvix.Core.IR.Evaluator.Types
import Juvix.Core.IR.Evaluator.Weak
import Juvix.Core.IR.TransformExt
import qualified Juvix.Core.IR.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library
import qualified Juvix.Library.Usage as Usage

type NoExtensions ext primTy primVal =
  ( IR.TermX ext primTy primVal ~ Void,
    IR.ElimX ext primTy primVal ~ Void
  )

type EvalPatSubst ext primTy primVal =
  ( HasPatSubst (OnlyExts.T ext) primTy primVal (IR.TermX ext primTy primVal),
    HasPatSubst (OnlyExts.T ext) primTy primVal (IR.ElimX ext primTy primVal),
    -- FIXME?
    HasPatSubstTerm (OnlyExts.T ext) primTy primVal primTy,
    HasPatSubstTerm (OnlyExts.T ext) primTy primVal primVal
  )

-- |
-- * @extT@: extension of current term
-- * @extG@: extension of terms in globals
type CanEval extT extG primTy primVal =
  ( Param.CanApply primTy,
    Param.CanApply primVal,
    EvalPatSubst extT primTy primVal,
    -- no extensions (only annotations) allowed in global context
    NoExtensions extG primTy primVal,
    HasSubstValue IR.NoExt primTy primVal primTy,
    HasSubstValue IR.NoExt primTy primVal primVal
  )

inlineAllGlobals ::
  ( EvalPatSubst ext primTy primVal,
    NoExtensions ext primTy primVal
  ) =>
  IR.Term' ext primTy primVal ->
  LookupFun ext primTy primVal ->
  IR.Term' ext primTy primVal
inlineAllGlobals t map =
  case t of
    IR.Unit' {} -> t
    IR.UnitTy' {} -> t
    IR.Pair' p1 p2 ann ->
      IR.Pair' (inlineAllGlobals p1 map) (inlineAllGlobals p2 map) ann
    IR.Elim' elim ann ->
      IR.Elim' (inlineAllGlobalsElim elim map) ann
    IR.Sig' u t1 t2 ann ->
      IR.Sig' u (inlineAllGlobals t1 map) (inlineAllGlobals t2 map) ann
    IR.Let' u e t ann ->
      IR.Let' u (inlineAllGlobalsElim e map) (inlineAllGlobals t map) ann
    IR.Lam' t ann ->
      IR.Lam' (inlineAllGlobals t map) ann
    IR.Pi' u t1 t2 ann ->
      IR.Pi' u (inlineAllGlobals t1 map) (inlineAllGlobals t2 map) ann
    IR.Prim' {} -> t
    IR.PrimTy' {} -> t
    IR.Star' {} -> t

inlineAllGlobalsElim ::
  ( EvalPatSubst ext primTy primVal,
    NoExtensions ext primTy primVal
  ) =>
  IR.Elim' ext primTy primVal ->
  LookupFun ext primTy primVal ->
  IR.Elim' ext primTy primVal
inlineAllGlobalsElim t map =
  case t of
    IR.Bound' {} -> t
    IR.Free' (IR.Global name) _ann -> fromMaybe t $ map name
    IR.Free' {} -> t
    IR.App' elim term ann ->
      IR.App' (inlineAllGlobalsElim elim map) (inlineAllGlobals term map) ann
    IR.Ann' u t1 t2 uni ann ->
      IR.Ann' u (inlineAllGlobals t1 map) (inlineAllGlobals t2 map) uni ann

-- annotations are discarded
evalTermWith ::
  CanEval extT extG primTy primVal =>
  LookupFun extG primTy primVal ->
  ExtFuns extG extT primTy primVal ->
  IR.Term' (OnlyExts.T extT) primTy primVal ->
  Either (Error IR.NoExt extT primTy primVal) (IR.Value primTy primVal)
evalTermWith _ _ (IR.Star' u _) =
  pure $ IR.VStar u
evalTermWith _ _ (IR.PrimTy' p _) =
  pure $ IR.VPrimTy p
evalTermWith _ _ (IR.Prim' p _) =
  pure $ IR.VPrim p
evalTermWith g exts (IR.Pi' π s t _) =
  IR.VPi π <$> evalTermWith g exts s <*> evalTermWith g exts t
evalTermWith g exts (IR.Lam' t _) =
  IR.VLam <$> evalTermWith g exts t
evalTermWith g exts (IR.Sig' π s t _) =
  IR.VSig π <$> evalTermWith g exts s <*> evalTermWith g exts t
evalTermWith g exts (IR.Pair' s t _) =
  IR.VPair <$> evalTermWith g exts s <*> evalTermWith g exts t
evalTermWith _ _ (IR.UnitTy' _) =
  pure IR.VUnitTy
evalTermWith _ _ (IR.Unit' _) =
  pure IR.VUnit
evalTermWith g exts (IR.Let' _ l b _) = do
  l' <- evalElimWith g exts l
  b' <- evalTermWith g exts b
  substV l' b'
evalTermWith g exts (IR.Elim' e _) =
  evalElimWith g exts e
evalTermWith g exts (IR.TermX a) =
  tExtFun exts g a

evalElimWith ::
  CanEval extT extG primTy primVal =>
  LookupFun extG primTy primVal ->
  ExtFuns extG extT primTy primVal ->
  IR.Elim' (OnlyExts.T extT) primTy primVal ->
  Either (Error IR.NoExt extT primTy primVal) (IR.Value primTy primVal)
evalElimWith _ _ (IR.Bound' i _) =
  pure $ IR.VBound i
evalElimWith g exts (IR.Free' x _)
  | IR.Global x <- x,
    Just e <- g x =
    evalElimWith g exts $ toOnlyExtsE e
  | otherwise = pure $ IR.VFree x
evalElimWith g exts (IR.App' s t _) =
  join $
    vapp <$> evalElimWith g exts s
      <*> evalTermWith g exts t
      <*> pure ()
evalElimWith g exts (IR.Ann' _ s _ _ _) =
  evalTermWith g exts s
evalElimWith g exts (IR.ElimX a) =
  eExtFun exts g a

evalTerm ::
  CanEval extT extG primTy primVal =>
  LookupFun extG primTy primVal ->
  IR.Term' extT primTy primVal ->
  Either (Error IR.NoExt extT primTy primVal) (IR.Value primTy primVal)
evalTerm g t = evalTermWith g rejectExts $ OnlyExts.onlyExtsT t

evalElim ::
  CanEval extT extG primTy primVal =>
  LookupFun extG primTy primVal ->
  IR.Elim' extT primTy primVal ->
  Either (Error IR.NoExt extT primTy primVal) (IR.Value primTy primVal)
evalElim g e = evalElimWith g rejectExts $ OnlyExts.onlyExtsE e

-- TODO generalise the @IR.NoExt@s
toLambda' ::
  forall ext' ext primTy primVal.
  ( EvalPatSubst ext' primTy primVal,
    NoExtensions ext primTy primVal
  ) =>
  IR.GlobalUsage ->
  IR.Term primTy primVal ->
  [IR.Pattern' ext primTy primVal] ->
  IR.Term' ext primTy primVal ->
  Maybe (IR.Elim' (OnlyExts.T ext') primTy primVal)
toLambda' π' ty' pats rhs = do
  patVars <- traverse singleVar pats
  let len = fromIntegral $ length patVars
  let vars = map bound $ genericTake len (iterate (subtract 1) (len - 1))
  let patMap = IntMap.fromList $ zip patVars vars
  let π = IR.globalToUsage π'
  let ty = OnlyExts.injectT ty'
  case patSubst patMap $ weakBy len $ toOnlyExtsT rhs of
    Left _ -> Nothing
    Right x -> pure $ IR.Ann π (applyN len lam x) ty 0 -- FIXME universe
  where
    applyN 0 _ x = x
    applyN n f x = applyN (n - 1) f (f $! x)
    bound :: IR.BoundVar -> IR.Elim' (OnlyExts.T ext') primTy primVal
    bound x = IR.Bound' x ()
    lam ::
      IR.Term' (OnlyExts.T z) primTy primVal ->
      IR.Term' (OnlyExts.T z) primTy primVal
    lam x = IR.Lam' x ()

singleVar :: Alternative f => IR.Pattern' ext primTy primVal -> f IR.PatternVar
singleVar (IR.PVar' p _) = pure p
singleVar _ = empty

toOnlyExtsT ::
  NoExtensions ext1 primTy primVal =>
  IR.Term' ext1 primTy primVal ->
  IR.Term' (OnlyExts.T ext2) primTy primVal
toOnlyExtsT = extTransformT $ OnlyExts.injector `compose` forgetter

toOnlyExtsE ::
  NoExtensions ext1 primTy primVal =>
  IR.Elim' ext1 primTy primVal ->
  IR.Elim' (OnlyExts.T ext2) primTy primVal
toOnlyExtsE = extTransformE $ OnlyExts.injector `compose` forgetter

toLambda ::
  forall ext ext' primTy primVal.
  ( EvalPatSubst ext' primTy primVal,
    NoExtensions ext primTy primVal
  ) =>
  IR.Global' IR.NoExt ext primTy primVal ->
  Maybe (IR.Elim' (OnlyExts.T ext') primTy primVal)
toLambda (IR.GFunction (IR.Function {funUsage = π, funType = ty, funClauses}))
  | IR.FunClause _ pats rhs _ _ _ :| [] <- funClauses =
    toLambda' π (IR.quote ty) pats rhs
toLambda _ = Nothing

toLambdaR ::
  forall ext ext' primTy primVal.
  ( EvalPatSubst ext' primTy primVal,
    NoExtensions ext primTy primVal
  ) =>
  IR.RawGlobal' ext primTy primVal ->
  Maybe (IR.Elim' (OnlyExts.T ext') primTy primVal)
toLambdaR (IR.RawGFunction f)
  | IR.RawFunction {rawFunUsage = π, rawFunType = ty, rawFunClauses} <- f,
    IR.RawFunClause _ pats rhs _ :| [] <- rawFunClauses =
    toLambda' π (extForgetT ty) pats rhs
toLambdaR _ = Nothing

lookupFun ::
  forall ext' ext primTy primVal.
  ( EvalPatSubst ext' primTy primVal,
    NoExtensions ext primTy primVal
  ) =>
  IR.Globals' IR.NoExt ext primTy primVal ->
  LookupFun (OnlyExts.T ext') primTy primVal
lookupFun globals x =
  HashMap.lookup x globals >>= toLambda

rawLookupFun ::
  forall ext' ext primTy primVal.
  ( EvalPatSubst ext' primTy primVal,
    NoExtensions ext primTy primVal
  ) =>
  IR.RawGlobals' ext primTy primVal ->
  LookupFun (OnlyExts.T ext') primTy primVal
rawLookupFun globals x =
  HashMap.lookup x globals >>= toLambdaR

lookupFun' ::
  EvalPatSubst IR.NoExt primTy primVal =>
  IR.Globals primTy primVal ->
  LookupFun IR.NoExt primTy primVal
lookupFun' globals x = lookupFun @IR.NoExt globals x >>| extForgetE

rawLookupFun' ::
  EvalPatSubst IR.NoExt primTy primVal =>
  IR.RawGlobals primTy primVal ->
  LookupFun IR.NoExt primTy primVal
rawLookupFun' globals x = rawLookupFun @IR.NoExt globals x >>| extForgetE
