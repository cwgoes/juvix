{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.Core.Pipeline where

import qualified Data.HashMap.Strict as HM
import qualified Juvix.Backends.Michelson as Michelson
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import qualified Juvix.Core.ErasedAnn.Prim as Prim
import qualified Juvix.Core.Erasure as Erasure
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.Typechecker as TC
import qualified Juvix.Core.Translate as Translate
import qualified Juvix.Core.Types as Types
import Juvix.Library
import qualified Juvix.Library.Usage as Usage
import qualified Michelson.TypeCheck as Michelson
import qualified Michelson.Untyped as Michelson

type RawMichelson f = f Michelson.PrimTy Michelson.RawPrimVal

type RawMichelsonTerm = RawMichelson IR.Term

type RawMichelsonElim = RawMichelson IR.Elim

type MichelsonIR f = f Michelson.PrimTy Michelson.PrimValIR

type MichelsonHR f = f Michelson.PrimTy Michelson.PrimValHR

type MichelsonTerm = MichelsonHR HR.Term

type MichelsonComp res =
  forall m.
  MichelsonCompConstraints m =>
  RawMichelsonTerm ->
  Usage.T ->
  RawMichelsonTerm ->
  m res

type CompConstraints' primTy primVal compErr m =
  ( HasWriter "log" [Types.PipelineLog primTy primVal] m,
    HasReader "parameterisation" (Types.Parameterisation primTy primVal) m,
    HasThrow "error" (Types.PipelineError primTy primVal compErr) m,
    HasReader "globals" (IR.Globals primTy (Types.TypedPrim primTy primVal)) m
  )

type CompConstraints primTy primVal compErr m =
  ( CompConstraints' primTy primVal compErr m,
    Eq primTy,
    Eq primVal,
    Types.CanApply primTy,
    Types.CanApply (Types.TypedPrim primTy primVal),
    TC.PrimSubstValue primTy primVal,
    TC.PrimPatSubstTerm primTy primVal,
    IR.HasWeak primVal
  )

type MichelsonCompConstraints m =
  CompConstraints' Michelson.PrimTy Michelson.RawPrimVal Michelson.CompErr m

constMapPrim :: Erasure.MapPrim a a ty val
constMapPrim _ x = Right x

lookupMapPrim ::
  Erasure.MapPrim
    (Types.TypedPrim ty val)
    (ErasedAnn.TypedPrim ty val)
    ty
    (Types.TypedPrim ty val)
lookupMapPrim _ (App.Return ty tm) = pure $ App.Return ty tm
lookupMapPrim ns (App.Cont f xs n) =
  App.Cont f <$> traverse lookupArg xs <*> pure n
  where
    lookupArg (App.BoundArg i) =
      atMay ns (fromIntegral i)
        |> maybe (error i) (pure . App.VarArg)
    lookupArg (App.FreeArg x) = pure $ App.VarArg x
    lookupArg (App.TermArg t) = pure $ App.TermArg t
    error i =
      Left
        $ Erasure.InternalError
        $ "unknown de Bruijn index " <> show i

eraseGlobals ::
  MichelsonCompConstraints m =>
  m (Erasure.Globals Michelson.PrimTy Michelson.PrimValHR)
eraseGlobals = do
  globals <- ask @"globals"
  res <- for (HM.toList globals) \(key, value) ->
    Erasure.eraseGlobal value
      |> Erasure.exec constMapPrim lookupMapPrim
      |> either (throw @"error" . Types.ErasureError) (pure . (key,))
  pure (HM.fromList res)

coreToAnn ::
  MichelsonComp (ErasedAnn.AnnTerm Michelson.PrimTy Michelson.PrimValHR)
coreToAnn term usage ty = do
  -- FIXME: allow any universe!
  (term, _) <- typecheckErase' term usage ty
  pure $ ErasedAnn.convertTerm term usage

coreToMichelson :: MichelsonComp (Either Michelson.CompErr Michelson.EmptyInstr)
coreToMichelson term usage ty = do
  ann <- coreToAnn term usage ty
  pure $ fst $ Michelson.compileExpr $ toRaw ann

coreToMichelsonContract ::
  MichelsonComp (Either Michelson.CompErr (Michelson.Contract' Michelson.ExpandedOp, Michelson.SomeContract))
coreToMichelsonContract term usage ty = do
  ann <- coreToAnn term usage ty
  pure $ fst $ Michelson.compileContract $ toRaw ann

{-# DEPRECATED toRaw "TODO: use typed terms in michelson backend" #-}
toRaw :: Michelson.Term -> Michelson.RawTerm
toRaw t@(ErasedAnn.Ann {term}) = t {ErasedAnn.term = toRaw1 term}
  where
    toRaw1 (ErasedAnn.Var x) = ErasedAnn.Var x
    toRaw1 (ErasedAnn.Prim p) = primToRaw p
    toRaw1 t@(ErasedAnn.LamM {body}) = t {ErasedAnn.body = toRaw body}
    toRaw1 (ErasedAnn.PairM l r) = ErasedAnn.PairM (toRaw l) (toRaw r)
    toRaw1 ErasedAnn.UnitM = ErasedAnn.UnitM
    toRaw1 (ErasedAnn.AppM f xs) = ErasedAnn.AppM (toRaw f) (toRaw <$> xs)
    primToRaw (App.Return {retTerm}) = ErasedAnn.Prim retTerm
    primToRaw (App.Cont {fun, args}) =
      ErasedAnn.AppM (takeToTerm fun) (argsToTerms (App.type' fun) args)
    takeToTerm (App.Take {usage, type', term}) =
      ErasedAnn.Ann
        { usage,
          type' = Prim.fromPrimType type',
          term = ErasedAnn.Prim term
        }
    argsToTerms ts xs = go (toList ts) xs
      where
        go _ [] = []
        go (_ : ts) (App.TermArg a : as) =
          takeToTerm a : go ts as
        go (t : ts) (App.VarArg x : as) =
          varTerm t x : go ts as
        go [] (_ : _) =
          -- a well typed application can't have more arguments than arrows
          undefined
        varTerm t x =
          ErasedAnn.Ann
            { usage = Usage.Omega, -- FIXME should usages even exist after erasure?
              type' = ErasedAnn.PrimTy t,
              term = ErasedAnn.Var x
            }

-- For interaction net evaluation, includes elementary affine check
-- , requires MonadIO for Z3.
-- FIXME
-- typecheckAffineErase ::
--   ( HasWriter "log" [Types.PipelineLog primTy primVal] m,
--     HasReader "parameterisation" (Types.Parameterisation primTy primVal) m,
--     HasThrow "error" (Types.PipelineError primTy primVal compErr) m,
--     HasReader "globals" (IR.Globals primTy primVal) m,
--     MonadIO m,
--     Eq primTy,
--     Eq primVal,
--     Show primTy,
--     Show primVal,
--     Show compErr
--   ) =>
--   HR.Term primTy primVal ->
--   Usage.T ->
--   HR.Term primTy primVal ->
--   m (Types.TermAssignment primTy primVal compErr)
-- typecheckAffineErase term usage ty = do
--   -- First typecheck & generate erased core.
--   (Types.WithType termAssign _type') <- typecheckErase term usage ty
--   -- Fetch the parameterisation, needed for EAC inference
--   -- TODO ∷ get rid of this dependency.
--   parameterisation <- ask @"parameterisation"
--   -- Then invoke Z3 to check elementary-affine-ness.
--   start <- liftIO unixTime
--   result <- liftIO (EAC.validEal parameterisation termAssign)
--   end <- liftIO unixTime
--   tell @"log" [Types.LogRanZ3 (end - start)]
--   -- Return accordingly.
--   case result of
--     Right (eac, _) -> do
--       let erasedEac = EAC.erase eac
--       unless
--         (erasedEac == Types.term termAssign)
--         ( throw @"error"
--             ( Types.InternalInconsistencyError
--                 "erased affine core should always match erased core"
--             )
--         )
--       pure termAssign
--     Left err -> throw @"error" (Types.EACError err)

-- For standard evaluation, no elementary affine check, no MonadIO required.
typecheckEval ::
  CompConstraints primTy primVal compErr m =>
  HR.Term primTy primVal ->
  Usage.T ->
  IR.Value primTy (Types.TypedPrim primTy primVal) ->
  m (IR.Value primTy (Types.TypedPrim primTy primVal))
typecheckEval term usage ty = do
  -- Fetch the parameterisation, needed for typechecking.
  param <- ask @"parameterisation"
  globals <- ask @"globals"
  -- First convert HR to IR.
  let irTerm = Translate.hrToIR term
  tell @"log" [Types.LogHRtoIR term irTerm]
  -- Typecheck & return accordingly.
  case IR.typeTerm param irTerm (IR.Annotation usage ty)
    >>= IR.evalTC
    |> IR.execTC globals
    |> fst of
    Right value -> pure value
    Left err -> throw @"error" (Types.TypecheckerError err)

-- For standard evaluation, no elementary affine check, no MonadIO required.
typecheckErase' ::
  CompConstraints primTy primVal compErr m =>
  IR.Term primTy primVal ->
  Usage.T ->
  IR.Term primTy primVal ->
  m
    ( Erasure.Term primTy (ErasedAnn.TypedPrim primTy primVal),
      IR.Value primTy (Types.TypedPrim primTy primVal)
    )
typecheckErase' term usage ty = do
  ty <- typecheckEval (Translate.irToHR ty) (Usage.SNat 0) (IR.VStar 0)
  term <- typecheckErase term usage ty
  pure (term, ty)

-- For standard evaluation, no elementary affine check, no MonadIO required.
typecheckErase ::
  CompConstraints primTy primVal compErr m =>
  IR.Term primTy primVal ->
  Usage.T ->
  IR.Value primTy (Types.TypedPrim primTy primVal) ->
  m (Erasure.Term primTy (ErasedAnn.TypedPrim primTy primVal))
typecheckErase term usage ty = do
  -- Fetch the parameterisation, needed for typechecking.
  param <- ask @"parameterisation"
  globals <- ask @"globals"
  -- Typecheck & return accordingly.
  case IR.typeTerm param term (IR.Annotation usage ty)
    |> IR.execTC globals
    |> fst of
    Right tyTerm -> do
      case Erasure.erase constMapPrim lookupMapPrim tyTerm usage of
        Right res -> pure res
        Left err -> throw @"error" (Types.ErasureError err)
    Left err -> throw @"error" (Types.TypecheckerError err)
