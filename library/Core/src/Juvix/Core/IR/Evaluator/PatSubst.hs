{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.IR.Evaluator.PatSubst where

import Data.Foldable (foldr1) -- on NonEmpty
import qualified Data.IntMap as IntMap
import qualified Juvix.Core.Application as App
import Juvix.Core.IR.Evaluator.Weak
import qualified Juvix.Core.IR.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library
import qualified Juvix.Library.Usage as Usage

class HasWeak a => HasPatSubst extT primTy primVal a where
  -- returns either a substituted term or an unbound pattern var
  -- TODO: use @validation@ to return all unbound vars
  patSubst' ::
    -- | How many bindings have been traversed so far
    Natural ->
    -- | Mapping of pattern variables to matched subterms
    IR.PatternMap (IR.Elim' extT primTy primVal) ->
    a ->
    Either IR.PatternVar a
  default patSubst' ::
    ( Generic a,
      GHasPatSubst extT primTy primVal (Rep a)
    ) =>
    Natural ->
    IR.PatternMap (IR.Elim' extT primTy primVal) ->
    a ->
    Either IR.PatternVar a
  patSubst' b m = fmap to . gpatSubst' b m . from

patSubst ::
  (HasPatSubst extT primTy primVal a) =>
  IR.PatternMap (IR.Elim' extT primTy primVal) ->
  a ->
  Either IR.PatternVar a
patSubst = patSubst' 0

class HasWeak a => HasPatSubstTerm extT primTy primVal a where
  -- returns either a substituted term or an unbound pattern var
  -- TODO: use @validation@ to return all unbound vars
  patSubstTerm' ::
    -- | How many bindings have been traversed so far
    Natural ->
    -- | Mapping of pattern variables to matched subterms
    IR.PatternMap (IR.Elim' extT primTy primVal) ->
    a ->
    Either IR.PatternVar (IR.Term' extT primTy primVal)

patSubstTerm ::
  (HasPatSubstTerm extT primTy primVal a) =>
  IR.PatternMap (IR.Elim' extT primTy primVal) ->
  a ->
  Either IR.PatternVar (IR.Term' extT primTy primVal)
patSubstTerm = patSubstTerm' 0

type AllPatSubst ext primTy primVal =
  ( IR.TermAll (HasPatSubst ext primTy primVal) ext primTy primVal,
    IR.ElimAll (HasPatSubst ext primTy primVal) ext primTy primVal,
    HasPatSubstTerm ext primTy primVal primTy,
    HasPatSubstTerm ext primTy primVal primVal
  )

instance
  AllPatSubst ext primTy primVal =>
  HasPatSubst ext primTy primVal (IR.Term' ext primTy primVal)
  where
  patSubst' b m (IR.Star' u a) =
    IR.Star' u <$> patSubst' b m a
  patSubst' b m (IR.PrimTy' t _) =
    -- FIXME annotation?
    patSubstTerm' b m t
  patSubst' b m (IR.Prim' p _) =
    -- FIXME annotation?
    patSubstTerm' b m p
  patSubst' b m (IR.Pi' π s t a) =
    IR.Pi' π <$> patSubst' b m s
      <*> patSubst' (succ b) m t
      <*> patSubst' b m a
  patSubst' b m (IR.Lam' t a) =
    IR.Lam' <$> patSubst' (succ b) m t
      <*> patSubst' b m a
  patSubst' b m (IR.Sig' π s t a) =
    IR.Sig' π <$> patSubst' b m s
      <*> patSubst' (succ b) m t
      <*> patSubst' b m a
  patSubst' b m (IR.Pair' s t a) =
    IR.Pair' <$> patSubst' b m s
      <*> patSubst' b m t
      <*> patSubst' b m a
  patSubst' b m (IR.UnitTy' a) =
    IR.UnitTy' <$> patSubst' b m a
  patSubst' b m (IR.Unit' a) =
    IR.Unit' <$> patSubst' b m a
  patSubst' b m (IR.Let' π l t a) =
    IR.Let' π <$> patSubst' b m l
      <*> patSubst' (succ b) m t
      <*> patSubst' b m a
  patSubst' b m (IR.Elim' e a) =
    IR.Elim' <$> patSubst' b m e
      <*> patSubst' b m a
  patSubst' b m (IR.TermX a) =
    IR.TermX <$> patSubst' b m a

instance
  AllPatSubst ext primTy primVal =>
  HasPatSubst ext primTy primVal (IR.Elim' ext primTy primVal)
  where
  patSubst' b m (IR.Bound' j a) =
    IR.Bound' j <$> patSubst' b m a
  patSubst' b m (IR.Free' (IR.Pattern x) _) =
    case IntMap.lookup x m of
      Nothing -> Left x
      Just e -> pure $ weakBy b e
  patSubst' b m (IR.Free' x a) =
    IR.Free' x <$> patSubst' b m a
  patSubst' b m (IR.App' f e a) =
    IR.App' <$> patSubst' b m f
      <*> patSubst' b m e
      <*> patSubst' b m a
  patSubst' b m (IR.Ann' π s t ℓ a) =
    IR.Ann' π <$> patSubst' b m s
      <*> patSubst' b m t
      <*> pure ℓ
      <*> patSubst' b m a
  patSubst' b m (IR.ElimX a) =
    IR.ElimX <$> patSubst' b m a

class GHasWeak f => GHasPatSubst extT primTy primVal f where
  gpatSubst' ::
    -- | How many bindings have been traversed so far
    Natural ->
    -- | Mapping of pattern variables to matched subterms
    IR.PatternMap (IR.Elim' extT primTy primVal) ->
    f t ->
    Either IR.PatternVar (f t)

instance GHasPatSubst ext primTy primVal U1 where gpatSubst' _ _ U1 = pure U1

instance GHasPatSubst ext primTy primVal V1 where
  gpatSubst' _ _ v = case v of

instance
  ( GHasPatSubst ext primTy primVal f,
    GHasPatSubst ext primTy primVal g
  ) =>
  GHasPatSubst ext primTy primVal (f :*: g)
  where
  gpatSubst' b m (x :*: y) =
    (:*:) <$> gpatSubst' b m x
      <*> gpatSubst' b m y

instance
  ( GHasPatSubst ext primTy primVal f,
    GHasPatSubst ext primTy primVal g
  ) =>
  GHasPatSubst ext primTy primVal (f :+: g)
  where
  gpatSubst' b m (L1 x) = L1 <$> gpatSubst' b m x
  gpatSubst' b m (R1 x) = R1 <$> gpatSubst' b m x

instance
  GHasPatSubst ext primTy primVal f =>
  GHasPatSubst ext primTy primVal (M1 i t f)
  where
  gpatSubst' b m (M1 x) = M1 <$> gpatSubst' b m x

instance
  HasPatSubst ext primTy primVal f =>
  GHasPatSubst ext primTy primVal (K1 k f)
  where
  gpatSubst' b m (K1 x) = K1 <$> patSubst' b m x

instance HasPatSubst ext primTy primVal ()

instance HasPatSubst ext primTy primVal Void

instance HasPatSubst ext primTy primVal Natural where
  patSubst' _ _ n = pure n

instance HasPatSubst ext primTy primVal Usage.T where
  patSubst' _ _ π = pure π

instance
  ( HasPatSubst ext primTy primVal a,
    HasPatSubst ext primTy primVal b
  ) =>
  HasPatSubst ext primTy primVal (a, b)

instance
  ( HasPatSubst ext primTy primVal a,
    HasPatSubst ext primTy primVal b,
    HasPatSubst ext primTy primVal c
  ) =>
  HasPatSubst ext primTy primVal (a, b, c)

instance
  ( HasPatSubst ext primTy primVal a,
    HasPatSubst ext primTy primVal b
  ) =>
  HasPatSubst ext primTy primVal (Either a b)

instance
  HasPatSubst ext primTy primVal a =>
  HasPatSubst ext primTy primVal (Maybe a)

instance
  HasPatSubst ext primTy primVal a =>
  HasPatSubst ext primTy primVal [a]

instance
  HasPatSubst ext primTy primVal a =>
  HasPatSubst ext primTy primVal (NonEmpty a)

instance
  ( HasPatSubst ext primTy primVal ty,
    HasPatSubst ext primTy primVal term
  ) =>
  HasPatSubst ext primTy primVal (App.Take ty term)

instance
  ( HasPatSubst ext primTy primVal term,
    HasPatSubst ext primTy primVal ty,
    HasPatSubst ext primTy primVal (App.ParamVar ext)
  ) =>
  HasPatSubst ext primTy primVal (App.Arg' ext ty term)

instance
  ( HasPatSubst ext primTy primVal ty,
    HasPatSubst ext primTy primVal term,
    HasPatSubst ext primTy primVal (App.ParamVar ext)
  ) =>
  HasPatSubst ext primTy primVal (App.Return' ext ty term)

instance
  ( HasWeak primTy,
    HasWeak primVal
  ) =>
  HasPatSubstTerm
    (OnlyExts.T ext)
    primTy
    (Param.TypedPrim primTy primVal)
    (Param.TypedPrim primTy primVal)
  where
  -- FIXME pat vars can't yet show up here
  patSubstTerm' _ _ (App.Cont {fun, args}) =
    pure $ IR.Elim $ foldl IR.App (takeToElim fun) (map argToTerm args)
  patSubstTerm' _ _ ret@(App.Return {}) =
    pure $ IR.Prim ret

takeToElim ::
  App.Take (Param.PrimType primTy) primVal ->
  IR.Elim' (OnlyExts.T ext) primTy (Param.TypedPrim primTy primVal)
takeToElim (App.Take {type', term}) =
  let term' = IR.Prim (App.Return {retType = type', retTerm = term})
      ty' = typeToTerm type'
   in IR.Ann Usage.Omega term' ty' 0

argToTerm ::
  App.Arg (Param.PrimType primTy) primVal ->
  IR.Term' (OnlyExts.T ext) primTy (Param.TypedPrim primTy primVal)
argToTerm = \case
  App.TermArg (App.Take {type', term}) ->
    IR.Prim $ App.Return {retType = type', retTerm = term}
  App.BoundArg i -> IR.Elim $ IR.Bound i
  App.FreeArg x -> IR.Elim $ IR.Free $ IR.Global x

typeToTerm ::
  ( Monoid (IR.XPi ext primTy primVal),
    Monoid (IR.XPrimTy ext primTy primVal)
  ) =>
  Param.PrimType primTy ->
  IR.Term' ext primTy primVal
typeToTerm tys = foldr1 arr $ map prim tys
  where
    prim ty = IR.PrimTy' ty mempty
    arr s t = IR.Pi' Usage.Omega s t mempty
