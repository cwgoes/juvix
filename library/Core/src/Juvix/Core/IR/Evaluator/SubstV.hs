{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.IR.Evaluator.SubstV where

import Data.Foldable (foldr1) -- on NonEmpty
import qualified Juvix.Core.Application as App
import Juvix.Core.IR.Evaluator.Types
import Juvix.Core.IR.Evaluator.Weak
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library
import qualified Juvix.Library.Usage as Usage

class HasWeak a => HasSubstV extV primTy primVal a where
  substVWith ::
    Natural ->
    IR.BoundVar ->
    IR.Value' extV primTy primVal ->
    a ->
    Either (Error extV extT primTy primVal) a
  default substVWith ::
    ( Generic a,
      GHasSubstV extV primTy primVal (Rep a)
    ) =>
    Natural ->
    IR.BoundVar ->
    IR.Value' extV primTy primVal ->
    a ->
    Either (Error extV extT primTy primVal) a
  substVWith b i e = fmap to . gsubstVWith b i e . from

substV' ::
  HasSubstV extV primTy primVal a =>
  IR.BoundVar ->
  IR.Value' extV primTy primVal ->
  a ->
  Either (Error extV extT primTy primVal) a
substV' = substVWith 0

substV ::
  HasSubstV extV primTy primVal a =>
  IR.Value' extV primTy primVal ->
  a ->
  Either (Error extV extT primTy primVal) a
substV = substV' 0

class HasWeak a => HasSubstValue extV primTy primVal a where
  substValueWith ::
    Natural ->
    IR.BoundVar ->
    IR.Value' extV primTy primVal ->
    a ->
    Either (Error extV extT primTy primVal) (IR.Value' extV primTy primVal)

substValue' ::
  HasSubstValue extV primTy primVal a =>
  IR.BoundVar ->
  IR.Value' extV primTy primVal ->
  a ->
  Either (Error extV extT primTy primVal) (IR.Value' extV primTy primVal)
substValue' = substValueWith 0

substValue ::
  HasSubstValue extV primTy primVal a =>
  IR.Value' extV primTy primVal ->
  a ->
  Either (Error extV extT primTy primVal) (IR.Value' extV primTy primVal)
substValue = substValue' 0

type AllSubstV extV primTy primVal =
  ( IR.ValueAll (HasSubstV extV primTy primVal) extV primTy primVal,
    IR.NeutralAll (HasSubstV extV primTy primVal) extV primTy primVal,
    HasSubstValue extV primTy primVal primTy,
    HasSubstValue extV primTy primVal primVal,
    Param.CanApply primTy,
    Param.CanApply primVal
  )

instance
  ( AllSubstV extV primTy primVal,
    Monoid (IR.XVNeutral extV primTy primVal),
    Monoid (IR.XVLam extV primTy primVal),
    Monoid (IR.XVPrimTy extV primTy primVal),
    Monoid (IR.XVPrim extV primTy primVal)
  ) =>
  HasSubstV extV primTy primVal (IR.Value' extV primTy primVal)
  where
  substVWith w i e (IR.VStar' n a) =
    IR.VStar' n <$> substVWith w i e a
  substVWith w i e (IR.VPrimTy' p _) =
    -- TODO what about the annotation?
    substValueWith w i e p
  substVWith w i e (IR.VPi' π s t a) =
    IR.VPi' π <$> substVWith w i e s
      <*> substVWith (succ w) (succ i) e t
      <*> substVWith w i e a
  substVWith w i e (IR.VLam' t a) =
    IR.VLam' <$> substVWith (succ w) (succ i) e t
      <*> substVWith w i e a
  substVWith w i e (IR.VSig' π s t a) =
    IR.VSig' π <$> substVWith w i e s
      <*> substVWith (succ w) (succ i) e t
      <*> substVWith w i e a
  substVWith w i e (IR.VPair' s t a) =
    IR.VPair' <$> substVWith w i e s
      <*> substVWith w i e t
      <*> substVWith w i e a
  substVWith w i e (IR.VUnitTy' a) =
    IR.VUnitTy' <$> substVWith w i e a
  substVWith w i e (IR.VUnit' a) =
    IR.VUnit' <$> substVWith w i e a
  substVWith w i e (IR.VNeutral' n a) =
    substNeutralWith w i e n a
  substVWith w i e (IR.VPrim' p _) =
    -- TODO what about the annotation?
    substValueWith w i e p
  substVWith w i e (IR.ValueX a) =
    IR.ValueX <$> substVWith w i e a

-- (not quite an instance of @HasSubstValue@ because of the @XVNeutral@ stuff)
substNeutralWith ::
  ( AllSubstV extV primTy primVal,
    Monoid (IR.XVNeutral extV primTy primVal),
    Monoid (IR.XVLam extV primTy primVal),
    Monoid (IR.XVPrimTy extV primTy primVal),
    Monoid (IR.XVPrim extV primTy primVal)
  ) =>
  Natural ->
  IR.BoundVar ->
  IR.Value' extV primTy primVal ->
  IR.Neutral' extV primTy primVal ->
  IR.XVNeutral extV primTy primVal ->
  Either (Error extV extT primTy primVal) (IR.Value' extV primTy primVal)
-- not Neutral'!!!
substNeutralWith w i e (IR.NBound' j a) b = do
  a' <- substVWith w i e a
  b' <- substVWith w i e b
  pure $ case compare j i of
    LT -> IR.VNeutral' (IR.NBound' j a') b'
    EQ -> weakBy w e
    GT -> IR.VNeutral' (IR.NBound' (pred j) a') b'
substNeutralWith w i e (IR.NFree' x a) b =
  IR.VNeutral' <$> (IR.NFree' x <$> substVWith w i e a)
    <*> substVWith w i e b
substNeutralWith w i e (IR.NApp' f s a) _ =
  join $
    vapp <$> substNeutralWith w i e f mempty
      <*> substVWith w i e s
      <*> substVWith w i e a
substNeutralWith w i e (IR.NeutralX a) b =
  IR.VNeutral' <$> (IR.NeutralX <$> substVWith w i e a)
    <*> substVWith w i e b

vapp ::
  forall extV extT primTy primVal.
  ( AllSubstV extV primTy primVal,
    Monoid (IR.XVNeutral extV primTy primVal),
    Monoid (IR.XVLam extV primTy primVal),
    Monoid (IR.XVPrimTy extV primTy primVal),
    Monoid (IR.XVPrim extV primTy primVal)
  ) =>
  IR.Value' extV primTy primVal ->
  IR.Value' extV primTy primVal ->
  -- | the annotation to use if the result is another application node
  -- (if it isn't, then this annotation is unused)
  IR.XNApp extV primTy primVal ->
  Either (Error extV extT primTy primVal) (IR.Value' extV primTy primVal)
vapp s t ann =
  case s of
    IR.VLam' s _ -> substV t s
    IR.VNeutral' f _ -> pure $ IR.VNeutral' (IR.NApp' f s ann) mempty
    IR.VPrimTy' p _ -> case t of
      IR.VPrimTy' q _ ->
        app' ApplyErrorT IR.VPrimTy' (\_ -> Param.pureArg) p q
      IR.VNeutral' (IR.NFree' (IR.Global y) _) _ ->
        -- TODO pattern vars also
        app' ApplyErrorT IR.VPrimTy' Param.freeArg p y
      IR.VNeutral' (IR.NBound' i _) _ ->
        app' ApplyErrorT IR.VPrimTy' Param.boundArg p i
      _ ->
        Left $ CannotApply s t NoApplyError
    IR.VPrim' p _ -> case t of
      IR.VPrim' q _ ->
        app' ApplyErrorV IR.VPrim' (\_ -> Param.pureArg) p q
      IR.VNeutral' (IR.NFree' (IR.Global y) _) _ ->
        -- TODO pattern vars also
        app' ApplyErrorV IR.VPrim' Param.freeArg p y
      IR.VNeutral' (IR.NBound' i _) _ ->
        app' ApplyErrorV IR.VPrim' Param.boundArg p i
      _ ->
        Left $ CannotApply s t NoApplyError
    _ ->
      Left $ CannotApply s t NoApplyError
  where
    app' ::
      forall ann arg fun.
      (Param.CanApply fun, Monoid ann) =>
      (Param.ApplyError fun -> ApplyError primTy primVal) ->
      (fun -> ann -> IR.Value' extV primTy primVal) ->
      (Proxy fun -> arg -> Maybe (Param.Arg fun)) ->
      fun ->
      arg ->
      Either (Error extV extT primTy primVal) (IR.Value' extV primTy primVal)
    app' err con mkArg p y =
      case mkArg Proxy y of
        Nothing -> Left $ CannotApply s t NoApplyError
        Just y ->
          Param.apply1 p y |> bimap (CannotApply s t . err) (\r -> con r mempty)

class GHasWeak f => GHasSubstV extV primTy primVal f where
  gsubstVWith ::
    Natural ->
    IR.BoundVar ->
    IR.Value' extV primTy primVal ->
    f t ->
    Either (Error extV extT primTy primVal) (f t)

instance GHasSubstV ext primTy primVal U1 where gsubstVWith _ _ _ U1 = pure U1

instance GHasSubstV ext primTy primVal V1 where
  gsubstVWith _ _ _ v = case v of

instance
  ( GHasSubstV ext primTy primVal f,
    GHasSubstV ext primTy primVal g
  ) =>
  GHasSubstV ext primTy primVal (f :*: g)
  where
  gsubstVWith b i e (x :*: y) =
    (:*:) <$> gsubstVWith b i e x
      <*> gsubstVWith b i e y

instance
  ( GHasSubstV ext primTy primVal f,
    GHasSubstV ext primTy primVal g
  ) =>
  GHasSubstV ext primTy primVal (f :+: g)
  where
  gsubstVWith b i e (L1 x) = L1 <$> gsubstVWith b i e x
  gsubstVWith b i e (R1 x) = R1 <$> gsubstVWith b i e x

instance
  GHasSubstV ext primTy primVal f =>
  GHasSubstV ext primTy primVal (M1 i t f)
  where
  gsubstVWith b i e (M1 x) = M1 <$> gsubstVWith b i e x

instance
  HasSubstV ext primTy primVal f =>
  GHasSubstV ext primTy primVal (K1 k f)
  where
  gsubstVWith b i e (K1 x) = K1 <$> substVWith b i e x

instance HasSubstV ext primTy primVal ()

instance HasSubstV ext primTy primVal Void

instance HasSubstV ext primTy primVal Natural where
  substVWith _ _ _ n = pure n

instance HasSubstV ext primTy primVal Usage.T where
  substVWith _ _ _ π = pure π

instance
  ( HasSubstV ext primTy primVal a,
    HasSubstV ext primTy primVal b
  ) =>
  HasSubstV ext primTy primVal (a, b)

instance
  ( HasSubstV ext primTy primVal a,
    HasSubstV ext primTy primVal b,
    HasSubstV ext primTy primVal c
  ) =>
  HasSubstV ext primTy primVal (a, b, c)

instance
  ( HasSubstV ext primTy primVal a,
    HasSubstV ext primTy primVal b
  ) =>
  HasSubstV ext primTy primVal (Either a b)

instance
  HasSubstV ext primTy primVal a =>
  HasSubstV ext primTy primVal (Maybe a)

instance
  HasSubstV ext primTy primVal a =>
  HasSubstV ext primTy primVal [a]

instance
  HasSubstV ext primTy primVal a =>
  HasSubstV ext primTy primVal (NonEmpty a)

instance HasSubstV ext primTy primVal Symbol where
  substVWith _ _ _ x = pure x

instance
  ( AllSubstV extV primTy primVal,
    Monoid (IR.XVNeutral extV primTy primVal),
    Monoid (IR.XVLam extV primTy primVal),
    Monoid (IR.XVPrimTy extV primTy primVal),
    Monoid (IR.XVPrim extV primTy primVal)
  ) =>
  HasSubstValue extV primTy primVal (IR.Value' extV primTy primVal)
  where
  substValueWith = substVWith

instance
  ( AllSubstV ext primTy primVal,
    Monoid (IR.XNBound ext primTy primVal),
    Monoid (IR.XNFree ext primTy primVal),
    Monoid (IR.XVNeutral ext primTy primVal),
    Monoid (IR.XVLam ext primTy primVal),
    Monoid (IR.XVPrimTy ext primTy primVal),
    Monoid (IR.XVPrim ext primTy primVal)
  ) =>
  HasSubstValue ext primTy primVal App.DeBruijn
  where
  substValueWith b i e (App.BoundVar j) =
    substNeutralWith b i e (IR.NBound' j mempty) mempty
  substValueWith _ _ _ (App.FreeVar x) =
    pure $ IR.VNeutral' (IR.NFree' (IR.Global x) mempty) mempty

instance
  ( HasWeak ty,
    HasSubstValue ext primTy primVal term
  ) =>
  HasSubstValue ext primTy primVal (App.Take ty term)
  where
  substValueWith b i e (App.Take {term}) = substValueWith b i e term

instance
  ( HasSubstValue ext primTy primVal (App.ParamVar ext),
    HasSubstValue ext primTy primVal ty,
    HasSubstValue ext primTy primVal term
  ) =>
  HasSubstValue ext primTy primVal (App.Arg' ext ty term)
  where
  substValueWith b i e (App.VarArg x) = substValueWith b i e x
  substValueWith b i e (App.TermArg t) = substValueWith b i e t

instance
  ( HasSubstValue ext primTy primVal a,
    IR.ValueAll HasWeak ext primTy primVal,
    IR.NeutralAll HasWeak ext primTy primVal,
    HasWeak primTy,
    HasWeak primVal,
    Monoid (IR.XVPi ext primTy primVal)
  ) =>
  HasSubstValue ext primTy primVal (NonEmpty a)
  where
  substValueWith b i e tys =
    foldr1 pi <$> traverse (substValueWith b i e) tys
    where
      pi s t = IR.VPi' Usage.Omega s (weak t) mempty

-- TODO generalise @IR.NoExt@
instance
  ( HasWeak primTy,
    HasWeak primVal,
    HasSubstValue IR.NoExt primTy (Param.TypedPrim primTy primVal) primTy,
    Param.CanApply primTy,
    Param.CanApply (Param.TypedPrim primTy primVal)
  ) =>
  HasSubstValue
    IR.NoExt
    primTy
    (Param.TypedPrim primTy primVal)
    (Param.TypedPrim primTy primVal)
  where
  substValueWith b i e (App.Cont {fun, args}) = do
    let app f x = vapp f x ()
    let fun' = IR.VPrim (App.takeToReturn fun)
    args' <- traverse (substValueWith b i e . argToValue) args
    foldlM app fun' args'
  substValueWith _ _ _ ret@(App.Return {}) =
    pure $ IR.VPrim ret

argToValue ::
  App.Arg (Param.PrimType primTy) primVal ->
  IR.Value primTy (Param.TypedPrim primTy primVal)
argToValue = \case
  App.TermArg (App.Take {type', term}) ->
    IR.VPrim $ App.Return {retType = type', retTerm = term}
  App.BoundArg i -> IR.VBound i
  App.FreeArg x -> IR.VFree $ IR.Global x
