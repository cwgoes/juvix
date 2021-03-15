-- | This file contains the functions and aux functions to typecheck terms.
-- @typeTerm@ and @typeElim@ are called by functions for typechecking
-- datatype and function declarations.
module Juvix.Core.IR.CheckTerm
  ( module Juvix.Core.IR.CheckTerm,
    module Typed,
    module Env,
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IntMap
import Data.List.NonEmpty ((<|))
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.IR.Evaluator as Eval
import Juvix.Core.IR.Typechecker.Env as Env
import Juvix.Core.IR.Typechecker.Types as Typed
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library hiding (Datatype)
import qualified Juvix.Library.Usage as Usage

data Leftovers a
  = Leftovers
      { loValue :: a,
        loLocals :: UContext,
        loPatVars :: PatUsages
      }
  deriving (Eq, Show, Generic)

leftoversOk :: Leftovers a -> Bool
leftoversOk (Leftovers {loLocals, loPatVars}) =
  all leftoverOk loLocals && all leftoverOk loPatVars

leftoverOk :: Usage.T -> Bool
leftoverOk ρ = ρ == Usage.Omega || ρ == mempty

-- | Checks a 'Term' against an annotation and returns a decorated term if
-- successful.
typeTerm ::
  ( Eq primTy,
    Eq primVal,
    CanTC' ext primTy primVal m,
    Param.CanApply primTy,
    Param.CanApply (TypedPrim primTy primVal)
  ) =>
  Param.Parameterisation primTy primVal ->
  IR.Term' ext primTy primVal ->
  AnnotationT primTy primVal ->
  m (Typed.Term primTy primVal)
typeTerm param t ann = loValue <$> typeTermWith param IntMap.empty [] t ann

typeTermWith ::
  ( Eq primTy,
    Eq primVal,
    CanTC' ext primTy primVal m,
    Param.CanApply primTy,
    Param.CanApply (TypedPrim primTy primVal)
  ) =>
  Param.Parameterisation primTy primVal ->
  PatBinds primTy primVal ->
  Context primTy primVal ->
  IR.Term' ext primTy primVal ->
  AnnotationT primTy primVal ->
  m (Leftovers (Typed.Term primTy primVal))
typeTermWith param pats ctx t ann =
  execInner (withLeftovers $ typeTerm' t ann) (InnerState param pats ctx)

-- | Infers the type and usage for an 'Elim' and returns it decorated with this
-- information.
typeElim ::
  ( Eq primTy,
    Eq primVal,
    CanTC' ext primTy primVal m,
    Param.CanApply primTy,
    Param.CanApply (TypedPrim primTy primVal)
  ) =>
  Param.Parameterisation primTy primVal ->
  IR.Elim' ext primTy primVal ->
  Usage.T ->
  m (Typed.Elim primTy primVal)
typeElim param e σ =
  loValue <$> typeElimWith param IntMap.empty [] e σ

typeElimWith ::
  ( Eq primTy,
    Eq primVal,
    CanTC' ext primTy primVal m,
    Param.CanApply primTy,
    Param.CanApply (TypedPrim primTy primVal)
  ) =>
  Param.Parameterisation primTy primVal ->
  PatBinds primTy primVal ->
  Context primTy primVal ->
  IR.Elim' ext primTy primVal ->
  Usage.T ->
  m (Leftovers (Typed.Elim primTy primVal))
typeElimWith param pats ctx e σ =
  execInner (withLeftovers $ typeElim' e σ) (InnerState param pats ctx)

withLeftovers ::
  (HasBound primTy primVal m, HasPatBinds primTy primVal m) =>
  m a ->
  m (Leftovers a)
withLeftovers m =
  Leftovers <$> m
    <*> fmap (fmap annUsage) (get @"bound")
    <*> fmap (fmap annUsage) (get @"patBinds")

typeTerm' ::
  ( Eq primTy,
    Eq primVal,
    CanInnerTC' ext primTy primVal m,
    Param.CanApply primTy,
    Param.CanApply (TypedPrim primTy primVal)
  ) =>
  IR.Term' ext primTy primVal ->
  AnnotationT primTy primVal ->
  m (Typed.Term primTy primVal)
typeTerm' term ann@(Annotation σ ty) =
  case term of
    IR.Star' i _ -> do
      requireZero σ
      _j <- requireStar ty
      -- requireUniverseLT i j
      pure $ Typed.Star i ann
    IR.PrimTy' t _ -> do
      requireZero σ
      void $ requireStar ty
      pure $ Typed.PrimTy t ann
    IR.Prim' p _ -> do
      p' <- typePrim p ty
      pure $ Typed.Prim p' $ Annotation σ ty
    IR.Pi' π a b _ -> do
      requireZero σ
      void $ requireStar ty
      a' <- typeTerm' a ann
      b' <- typeTerm' b ann
      pure $ Typed.Pi π a' b' ann
    IR.Lam' t _ -> do
      (π, a, b) <- requirePi ty
      let varAnn = Annotation (σ <.> π) a
          tAnn = Annotation σ b
      t' <- withLocal varAnn $ typeTerm' t tAnn
      let anns = BindAnnotation {baBindAnn = varAnn, baResAnn = ann}
      pure $ Typed.Lam t' anns
    IR.Sig' π a b _ -> do
      requireZero σ
      void $ requireStar ty
      a' <- typeTerm' a ann
      b' <- typeTerm' b ann
      pure $ Typed.Sig π a' b' ann
    IR.Pair' s t _ -> do
      (π, a, b) <- requireSig ty
      let sAnn = Annotation (σ <.> π) a
      s' <- typeTerm' s sAnn
      tAnn <- Annotation σ <$> substApp b s'
      t' <- typeTerm' t tAnn
      pure $ Typed.Pair s' t' ann
    IR.UnitTy' _ -> do
      requireZero σ
      void $ requireStar ty
      pure $ Typed.UnitTy ann
    IR.Unit' _ -> do
      requireUnitTy ty
      pure $ Typed.Unit ann
    IR.Let' σb b t _ -> do
      b' <- typeElim' b σb
      let bAnn = getElimAnn b'
          tAnn = Annotation σ (Eval.weak ty)
      t' <- withLocal bAnn $ typeTerm' t tAnn
      let anns = BindAnnotation {baBindAnn = bAnn, baResAnn = ann}
      pure $ Typed.Let σb b' t' anns
    IR.Elim' e _ -> do
      e' <- typeElim' e σ
      let ty' = annType $ getElimAnn e'
      requireSubtype e ty ty'
      pure $ Typed.Elim e' ann
    IR.TermX x ->
      throwTC $ UnsupportedTermExt x

typeElim' ::
  ( Eq primTy,
    Eq primVal,
    CanInnerTC' ext primTy primVal m,
    Param.CanApply primTy,
    Param.CanApply (TypedPrim primTy primVal)
  ) =>
  IR.Elim' ext primTy primVal ->
  Usage.T ->
  m (Typed.Elim primTy primVal)
typeElim' elim σ =
  case elim of
    IR.Bound' i _ -> do
      ty <- useLocal σ i
      pure $ Typed.Bound i $ Annotation σ ty
    IR.Free' px@(IR.Pattern x) _ -> do
      ty <- usePatVar σ x
      pure $ Typed.Free px $ Annotation σ ty
    IR.Free' gx@(IR.Global x) _ -> do
      (ty, π') <- lookupGlobal x
      when (π' == IR.GZero) $ requireZero σ
      pure $ Typed.Free gx $ Annotation σ ty
    IR.App' s t _ -> do
      s' <- typeElim' s σ
      (π, a, b) <- requirePi $ annType $ getElimAnn s'
      let tAnn = Annotation (σ <.> π) a
      t' <- typeTerm' t tAnn
      ty <- substApp b t'
      pure $ Typed.App s' t' $ Annotation σ ty
    IR.Ann' π s a ℓ _ -> do
      a' <- typeTerm' a $ Annotation mempty (IR.VStar ℓ)
      ty <- evalTC a'
      let ann = Annotation σ ty
      s' <- typeTerm' s ann
      pure $ Typed.Ann π s' a' ℓ ann
    IR.ElimX x ->
      throwTC $ UnsupportedElimExt x

pushLocal ::
  HasBound primTy primVal m =>
  AnnotationT primTy primVal ->
  m ()
pushLocal ann = modify @"bound" (ann :)

popLocal ::
  ( HasBound primTy primVal m,
    HasThrowTC' IR.NoExt ext primTy primVal m
  ) =>
  m ()
popLocal = do
  ctx <- get @"bound"
  case ctx of
    Annotation ρ _ : ctx -> do
      unless (leftoverOk ρ) $ throwTC (LeftoverUsage ρ)
      put @"bound" ctx
    [] -> do
      throwTC (UnboundIndex 0)

withLocal ::
  ( HasBound primTy primVal m,
    HasThrowTC' IR.NoExt ext primTy primVal m
  ) =>
  AnnotationT primTy primVal ->
  m a ->
  m a
withLocal ann m = pushLocal ann *> m <* popLocal

requireZero ::
  HasThrowTC' IR.NoExt ext primTy primVal m =>
  Usage.T ->
  m ()
requireZero π = unless (π == mempty) $ throwTC (UsageMustBeZero π)

requireStar ::
  HasThrowTC' IR.NoExt ext primTy primVal m =>
  Typed.ValueT primTy primVal ->
  m IR.Universe
requireStar (IR.VStar j) = pure j
requireStar ty = throwTC (ShouldBeStar ty)

requireUniverseLT ::
  HasThrowTC' IR.NoExt ext primTy primVal m =>
  IR.Universe ->
  IR.Universe ->
  m ()
requireUniverseLT i j = unless (i < j) $ throwTC (UniverseMismatch i j)

typePrim ::
  CanInnerTC' ext primTy primVal m =>
  primVal ->
  Typed.ValueT primTy primVal ->
  m (TypedPrim primTy primVal)
typePrim p ty = do
  param <- ask @"param"
  ty' <- toPrimTy ty
  if (Param.hasType param p ty')
    then pure $ App.Return {retType = ty', retTerm = p}
    else throwTC $ WrongPrimTy p ty'

toPrimTy ::
  CanInnerTC' ext primTy primVal m =>
  Typed.ValueT primTy primVal ->
  m (NonEmpty primTy)
toPrimTy ty = maybe (throwTC $ NotPrimTy ty) pure $ go ty
  where
    go (IR.VPrimTy t) = pure $ t :| []
    go (IR.VPi _ (IR.VPrimTy s) t) = (s <|) <$> go t
    go _ = empty

type TyParts primTy primVal =
  (Usage.T, Typed.ValueT primTy primVal, Typed.ValueT primTy primVal)

requirePi ::
  HasThrowTC' IR.NoExt ext primTy primVal m =>
  Typed.ValueT primTy primVal ->
  m (TyParts primTy primVal)
requirePi (IR.VPi π a b) = pure (π, a, b)
requirePi ty = throwTC (ShouldBeFunctionType ty)

requireSig ::
  HasThrowTC' IR.NoExt ext primTy primVal m =>
  Typed.ValueT primTy primVal ->
  m (TyParts primTy primVal)
requireSig (IR.VSig π a b) = pure (π, a, b)
requireSig ty = throwTC (ShouldBePairType ty)

requireUnitTy ::
  HasThrowTC' IR.NoExt ext primTy primVal m =>
  Typed.ValueT primTy primVal ->
  m ()
requireUnitTy IR.VUnitTy = pure ()
requireUnitTy ty = throwTC (ShouldBeUnitType ty)

requireSubtype ::
  (Eq primTy, Eq primVal, HasThrowTC' IR.NoExt ext primTy primVal m) =>
  IR.Elim' ext primTy primVal ->
  Typed.ValueT primTy primVal ->
  Typed.ValueT primTy primVal ->
  m ()
requireSubtype subj exp got =
  unless (got <: exp) $ throwTC (TypeMismatch subj exp got)

useLocal ::
  ( HasBound primTy primVal m,
    HasThrowTC' IR.NoExt ext primTy primVal m,
    Eval.HasWeak primTy,
    Eval.HasWeak primVal
  ) =>
  Usage.T ->
  IR.BoundVar ->
  m (Typed.ValueT primTy primVal)
useLocal π var = do
  ctx <- get @"bound"
  (ty, ctx) <- go 1 var ctx
  put @"bound" ctx
  pure ty
  where
    go _ _ [] = throwTC (UnboundIndex var)
    go w 0 (Annotation ρ ty : ctx) = do
      case ρ `Usage.minus` π of
        Just ρ' -> pure (Eval.weakBy w ty, Annotation ρ' ty : ctx)
        Nothing -> throwTC (InsufficientUsage π ρ)
    go w i (b : ctx) = second (b :) <$> go (w + 1) (i - 1) ctx

usePatVar ::
  ( HasPatBinds primTy primVal m,
    HasThrowTC' IR.NoExt ext primTy primVal m
  ) =>
  Usage.T ->
  IR.PatternVar ->
  m (Typed.ValueT primTy primVal)
usePatVar π var = do
  -- TODO a single traversal with alterF or something
  mAnn <- gets @"patBinds" $ IntMap.lookup var
  case mAnn of
    Just (Annotation ρ ty)
      | Just ρ' <- ρ `Usage.minus` π -> do
        modify @"patBinds" $ IntMap.insert var $ Annotation ρ' ty
        pure ty
      | otherwise -> do
        throwTC (InsufficientUsage π ρ)
    Nothing -> do
      throwTC (UnboundPatVar var)

liftEval ::
  HasThrowTC' extV extT primTy primVal m =>
  Either (Eval.Error IR.NoExt T primTy (TypedPrim primTy primVal)) a ->
  m a
liftEval = either (throwTC . EvalError) pure

substApp ::
  ( HasParam primTy primVal m,
    HasThrowTC' IR.NoExt extT primTy primVal m,
    HasGlobals primTy primVal m,
    Param.CanApply primTy,
    Param.CanApply (TypedPrim primTy primVal),
    PrimSubstValue primTy primVal,
    PrimPatSubstTerm primTy primVal
  ) =>
  Typed.ValueT primTy primVal ->
  Typed.Term primTy primVal ->
  m (Typed.ValueT primTy primVal)
substApp ty arg = do
  arg' <- evalTC arg
  liftEval $ Eval.substV arg' ty

evalTC ::
  ( HasThrowTC' IR.NoExt ext primTy primVal m,
    HasGlobals primTy primVal m,
    Param.CanApply primTy,
    Param.CanApply (TypedPrim primTy primVal),
    PrimSubstValue primTy primVal,
    PrimPatSubstTerm primTy primVal
  ) =>
  Typed.Term primTy primVal ->
  m (Typed.ValueT primTy primVal)
evalTC t = do
  g <- ask @"globals"
  liftEval $ Eval.evalTerm (Eval.lookupFun @T g) t

-- | Subtyping. If @s <: t@ then @s@ is a subtype of @t@, i.e. everything of
-- type @s@ can also be checked against type @t@.
--
-- Currently subtyping consists of the following:
--
-- * Consistency of universe levels (@*ᵢ <: *ⱼ@ if @i ≤ j@)
-- * Usage compatibility (@(π x: A) → B <: (ω x: A) → B@ for finite @π@)
-- * Contravariant domain & covariant codomain
--   (@(π x: A₁) → B₁ <: (π x: A₂) → B₂@ if
--    @A₂ <: A₁@ and @B₁ <: B₂@)
-- * Covariance in both parts of Σ
-- * It doesn't descend into any other structures
--   (TODO: which ones are safe to do so?)
--
-- NB. Levels are currently not checked!
(<:) ::
  ( Eq primTy,
    Eq primVal,
    IR.ValueAll Eq ext primTy primVal,
    IR.NeutralAll Eq ext primTy primVal
  ) =>
  IR.Value' ext primTy primVal ->
  IR.Value' ext primTy primVal ->
  Bool
IR.VStar' _i _ <: IR.VStar' _j _ = True -- i <= j
IR.VPi' π1 s1 t1 _ <: IR.VPi' π2 s2 t2 _ =
  π2 `Usage.allows` π1 && s2 <: s1 && t1 <: t2
IR.VSig' π1 s1 t1 _ <: IR.VSig' π2 s2 t2 _ =
  -- TODO is this right???
  π1 `Usage.allows` π2
    && s1 <: s2
    && t1 <: t2
s1 <: s2 = s1 == s2

infix 4 <: -- same as (<), etc
