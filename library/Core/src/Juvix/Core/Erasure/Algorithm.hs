module Juvix.Core.Erasure.Algorithm (erase, eraseAnn, eraseGlobal, exec) where

import Data.List (genericIndex)
import Juvix.Core.Erasure.Types (eraseAnn, exec)
import qualified Juvix.Core.Erasure.Types as Erasure
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.Typechecker.Types as Typed
import qualified Juvix.Core.IR.Types.Base as IR
import Juvix.Library hiding (empty)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

type ErasureM primTy1 primTy2 primVal1 primVal2 m =
  ( HasState "nextName" Int m,
    HasState "nameStack" [NameSymbol.T] m,
    HasThrow "erasureError" (Erasure.Error primTy1 primVal1) m,
    HasReader "mapPrimTy" (Erasure.MapPrim primTy1 primTy2 primTy1 primVal1) m,
    HasReader "mapPrimVal" (Erasure.MapPrim primVal1 primVal2 primTy1 primVal1) m
  )

erase ::
  (Erasure.MapPrim primTy1 primTy2 primTy1 primVal1) ->
  (Erasure.MapPrim primVal1 primVal2 primTy1 primVal1) ->
  Typed.Term' primTy1 primVal1 ->
  Usage.T ->
  Either (Erasure.Error primTy1 primVal1) (Erasure.Term primTy2 primVal2)
erase mt mv t π
  | π == mempty = Left $ Erasure.CannotEraseZeroUsageTerm t
  | otherwise = exec mt mv $ eraseTerm t

eraseGlobal ::
  ErasureM primTy1 primTy2 primVal1 primVal2 m =>
  IR.Global primTy1 primVal1 ->
  m (Erasure.Global primTy2 primVal2)
eraseGlobal g =
  case g of
    IR.GDatatype g -> Erasure.GDatatype |<< eraseDatatype g
    IR.GDataCon c -> Erasure.GDataCon |<< eraseDataCon c
    IR.GFunction f -> Erasure.GFunction |<< eraseFunction f
    -- TODO: Need the annotated term here. ref
    -- https://github.com/metastatedev/juvix/issues/495
    IR.GAbstract a -> Erasure.GAbstract |<< eraseAbstract a

eraseAbstract ::
  ErasureM primTy1 primTy2 primVal1 primVal2 m =>
  IR.Abstract primTy1 primVal1 ->
  m (Erasure.Abstract primTy2)
eraseAbstract (IR.Abstract name usage ty) =
  Erasure.Abstract name usage <$> eraseType ty

eraseDatatype ::
  ErasureM primTy1 primTy2 primVal1 primVal2 m =>
  IR.Datatype primTy1 primVal1 ->
  m (Erasure.Datatype primTy2 primVal2)
eraseDatatype (IR.Datatype name _pos args level cons) = do
  args <- mapM eraseDataArg args
  cons <- mapM eraseDataCon cons
  pure (Erasure.Datatype name args level cons)

eraseDataArg ::
  ErasureM primTy1 primTy2 primVal1 primVal2 m =>
  IR.DataArg primTy1 primVal1 ->
  m (Erasure.DataArg primTy2)
eraseDataArg (IR.DataArg name usage ty) = do
  ty <- eraseType ty
  pure (Erasure.DataArg name usage ty)

eraseDataCon ::
  ErasureM primTy1 primTy2 primVal1 primVal2 m =>
  IR.DataCon primTy1 primVal1 ->
  m (Erasure.DataCon primTy2 primVal2)
eraseDataCon (IR.DataCon name ty def) = do
  ty <- eraseType ty
  def <- traverse eraseFunction def
  pure (Erasure.DataCon name ty def)

eraseFunction ::
  ErasureM primTy1 primTy2 primVal1 primVal2 m =>
  IR.Function primTy1 primVal1 ->
  m (Erasure.Function primTy2 primVal2)
eraseFunction (IR.Function name usage ty clauses) = do
  let (tys, ret) = piTypeToList (IR.quote0 ty)
  clauses <- flip mapM clauses $ \(IR.FunClause _tel patts term _rhsTy _catchAll _unreachable) -> do
    let ty_ret = listToPiType (drop (length patts) tys, ret)
    (patts, ty) <- erasePatterns (patts, (tys, ret))
    patts <- mapM erasePattern patts
    -- TODO: Need the annotated term here. ref https://github.com/metastatedev/juvix/issues/495
    -- term <- eraseTerm term
    pure (Erasure.FunClause patts undefined)
  ty <- eraseType ty
  pure (Erasure.Function name usage ty clauses)

eraseFunClause ::
  ErasureM primTy1 primTy2 primVal1 primVal2 m =>
  IR.FunClause primTy1 primVal1 ->
  m b
eraseFunClause (IR.FunClause _tel patts term _rhsTy _catchAll _unreachable) = do
  patts <- mapM erasePattern patts
  -- TODO: Need the annotated term here. ref https://github.com/metastatedev/juvix/issues/495
  -- term <- eraseTerm term
  pure (Erasure.FunClause patts undefined)
  undefined

erasePattern ::
  ErasureM primTy1 primTy2 primVal1 primVal2 m =>
  IR.Pattern primTy1 primVal1 ->
  m (Erasure.Pattern primTy2 primVal2)
erasePattern patt =
  case patt of
    IR.PCon name patts -> do
      patts <- mapM erasePattern patts
      pure (Erasure.PCon name patts)
    IR.PPair l r ->
      Erasure.PPair <$> erasePattern l <*> erasePattern r
    IR.PUnit -> pure Erasure.PUnit
    IR.PVar v -> pure (Erasure.PVar v)
    IR.PDot _t -> do
      -- TODO: Need the annotated term here. ref https://github.com/metastatedev/juvix/issues/495
      -- t <- eraseTerm t
      -- pure (Erasure.PDot t)
      pure (Erasure.PDot undefined)
    IR.PPrim p -> Erasure.PPrim <$> erasePrimVal p

erasePrimTy ::
  ErasureM primTy1 primTy2 primVal1 primVal2 m => primTy1 -> m primTy2
erasePrimTy p = do
  ask @"mapPrimTy" <*> get @"nameStack" <*> pure p
    >>= either (throw @"erasureError") pure

erasePrimVal ::
  ErasureM primTy1 primTy2 primVal1 primVal2 m => primVal1 -> m primVal2
erasePrimVal p = do
  ask @"mapPrimVal" <*> get @"nameStack" <*> pure p
    >>= either (throw @"erasureError") pure

erasePatterns ::
  ErasureM primTy1 primTy2 primVal1 primVal2 m =>
  ([pat], ([(Usage.Usage, arg)], ret)) ->
  m ([pat], ([(Usage.Usage, arg)], ret))
erasePatterns ([], ([], ret)) = pure ([], ([], ret))
erasePatterns (_ : ps, ((Usage.SNat 0, _) : args, ret)) = erasePatterns (ps, (args, ret))
erasePatterns (p : ps, (arg : args, ret)) = do
  (ps', (args', ret')) <- erasePatterns (ps, (args, ret))
  pure (p : ps', (arg : args', ret'))
erasePatterns _ = throw @"erasureError" (Erasure.InternalError "invalid type & pattern match combination")

piTypeToList ::
  IR.Term primTy primVal ->
  ([(Usage.Usage, IR.Term primTy primVal)], IR.Term primTy primVal)
piTypeToList ty =
  case ty of
    IR.Pi usage arg ret ->
      let (rest, res) = piTypeToList ret in ((usage, arg) : rest, res)
    _ -> ([], ty)

listToPiType ::
  ([(Usage.Usage, IR.Term primTy primVal)], IR.Term primTy primVal) ->
  IR.Term primTy primVal
listToPiType ([], ret) = ret
listToPiType ((u, x) : xs, ret) = IR.Pi u x (listToPiType (xs, ret))

eraseTerm ::
  ErasureM primTy1 primTy2 primVal1 primVal2 m =>
  Typed.Term' primTy1 primVal1 ->
  m (Erasure.Term primTy2 primVal2)
eraseTerm t@(Typed.Star _ _) = throwEra $ Erasure.UnsupportedTermT t
eraseTerm t@(Typed.PrimTy _ _) = throwEra $ Erasure.UnsupportedTermT t
eraseTerm (Typed.Prim p ann) = do
  Erasure.Prim <$> erasePrimVal p <*> eraseType (IR.annType ann)
eraseTerm t@(Typed.Pi _ _ _ _) = throwEra $ Erasure.UnsupportedTermT t
eraseTerm (Typed.Lam t anns) = do
  let ty@(IR.VPi π _ _) = IR.annType $ IR.baResAnn anns
  (x, t) <- withName \x -> (x,) <$> eraseTerm t
  if π == mempty
    then pure t
    else Erasure.Lam x t <$> eraseType ty
eraseTerm t@(Typed.Sig {}) = throwEra $ Erasure.UnsupportedTermT t
eraseTerm (Typed.Pair s t ann) = do
  let ty@(IR.VSig π _ _) = IR.annType ann
  if π == mempty
    then eraseTerm t
    else Erasure.Pair <$> eraseTerm s <*> eraseTerm t <*> eraseType ty
eraseTerm t@(Typed.UnitTy {}) = throwEra $ Erasure.UnsupportedTermT t
eraseTerm (Typed.Unit ann) = Erasure.Unit <$> eraseType (IR.annType ann)
eraseTerm (Typed.Let π b t anns) = do
  (x, t) <- withName \x -> (x,) <$> eraseTerm t
  if π == mempty
    then pure t
    else do
      let exprTy = IR.annType $ IR.baResAnn anns
          bindTy = IR.annType $ IR.baBindAnn anns
      b <- eraseElim b
      bindTy <- eraseType bindTy
      exprTy <- eraseType exprTy
      pure (Erasure.Let x b t (bindTy, exprTy))
eraseTerm (Typed.Elim e _) = eraseElim e

eraseElim ::
  ErasureM primTy1 primTy2 primVal1 primVal2 m =>
  Typed.Elim' primTy1 primVal1 ->
  m (Erasure.Term primTy2 primVal2)
eraseElim (Typed.Bound x ann) = do
  Erasure.Var <$> lookupBound x
    <*> eraseType (IR.annType ann)
eraseElim (Typed.Free (IR.Global x) ann) = do
  Erasure.Var x <$> eraseType (IR.annType ann)
eraseElim e@(Typed.Free (IR.Pattern _) _) = do
  -- FIXME ??????
  throwEra $ Erasure.UnsupportedTermE e
eraseElim (Typed.App e s ann) = do
  let IR.VPi π _ _ = IR.annType $ IR.getElimAnn e
  e <- eraseElim e
  if π == mempty
    then pure e
    else do
      s <- eraseTerm s
      Erasure.App e s <$> eraseType (IR.annType ann)
eraseElim (Typed.Ann _ s _ _ _) = do
  eraseTerm s

eraseType ::
  ErasureM primTy1 primTy2 primVal1 primVal2 m =>
  IR.Value primTy1 primVal1 ->
  m (Erasure.Type primTy2)
eraseType (IR.VStar i) = do
  pure $ Erasure.Star i
eraseType (IR.VPrimTy t) = do
  Erasure.PrimTy <$> erasePrimTy t
eraseType (IR.VPi π a b) = do
  if π == mempty
    then eraseType b
    else-- FIXME dependency

    Erasure.Pi π <$> eraseType a
      <*> withName \_ -> eraseType b
eraseType v@(IR.VLam _) = do
  throwEra $ Erasure.UnsupportedTypeV v
eraseType (IR.VSig π a b) = do
  if π == mempty
    then eraseType a
    else Erasure.Sig π <$> eraseType a
      <*> withName \_ -> eraseType b
eraseType v@(IR.VPair _ _) = do
  throwEra $ Erasure.UnsupportedTypeV v
eraseType IR.VUnitTy = do
  pure Erasure.UnitTy
eraseType IR.VUnit = do
  throwEra $ Erasure.UnsupportedTypeV IR.VUnit
eraseType (IR.VNeutral n) = do
  eraseTypeN n
eraseType v@(IR.VPrim _) = do
  throwEra $ Erasure.UnsupportedTypeV v

eraseTypeN ::
  ErasureM primTy1 primTy2 primVal1 primVal2 m =>
  IR.Neutral primTy1 primVal1 ->
  m (Erasure.Type primTy2)
eraseTypeN (IR.NBound x) = do
  Erasure.SymT <$> lookupBound x
eraseTypeN (IR.NFree (IR.Global x)) = do
  pure $ Erasure.SymT x
eraseTypeN n@(IR.NFree (IR.Pattern _)) = do
  -- FIXME ??????
  throwEra $ Erasure.UnsupportedTypeN n
eraseTypeN n@(IR.NApp _ _) = do
  -- FIXME add AppT and fill this in
  throwEra $ Erasure.UnsupportedTypeN n

pushName ::
  (HasState "nextName" Int m, HasState "nameStack" [NameSymbol.T] m) =>
  m NameSymbol.T
pushName = do
  x <- gets @"nextName" $ NameSymbol.fromText . show
  modify @"nextName" succ
  modify @"nameStack" (x :)
  pure $ x

popName ::
  ( HasState "nameStack" [a] m,
    HasThrow "erasureError" (Erasure.Error primTy primVal) m
  ) =>
  m ()
popName = do
  ns <- get @"nameStack"
  case ns of
    [] -> throw @"erasureError" $ Erasure.InternalError "name stack ran out"
    _ : ns -> put @"nameStack" ns

withName ::
  ( HasState "nextName" Int m,
    HasState "nameStack" [NameSymbol.T] m,
    HasThrow "erasureError" (Erasure.Error primTy primVal) m
  ) =>
  (NameSymbol.T -> m a) ->
  m a
withName f = do x <- pushName; f x <* popName

lookupBound ::
  HasState "nameStack" [NameSymbol.T] m =>
  IR.BoundVar ->
  m NameSymbol.T
lookupBound x = gets @"nameStack" (`genericIndex` x)

throwEra ::
  HasThrow "erasureError" (Erasure.Error primTy primVal) m =>
  Erasure.Error primTy primVal ->
  m a
throwEra = throw @"erasureError"
