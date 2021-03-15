{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.IR.Typechecker.Env where

import qualified Data.HashMap.Strict as HashMap
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.TransformExt.OnlyExts as OnlyExts
import Juvix.Core.IR.Typechecker.Types
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.IR.Types.Globals as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library hiding (Datatype)
import qualified Juvix.Library.Usage as Usage

data EnvCtx' ext primTy primVal
  = EnvCtx
      { globals :: GlobalsT primTy primVal
      }
  deriving (Generic)

type EnvCtx = EnvCtx' IR.NoExt

type EnvAlias ext primTy primVal =
  ExceptT
    (TypecheckError' IR.NoExt ext primTy primVal)
    (State (EnvCtx' ext primTy primVal))

newtype EnvTypecheck' ext primTy primVal a
  = EnvTyp (EnvAlias ext primTy primVal a)
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasThrow "typecheckError" (TypecheckError' IR.NoExt ext primTy primVal)
    )
    via MonadError (EnvAlias ext primTy primVal)
  deriving
    ( HasSource "globals" (GlobalsT primTy primVal),
      HasReader "globals" (GlobalsT primTy primVal)
    )
    via ReaderField "globals" (EnvAlias ext primTy primVal)

type EnvTypecheck = EnvTypecheck' IR.NoExt

type HasGlobals primTy primVal = HasReader "globals" (GlobalsT primTy primVal)

type PrimSubstValue1 primTy primVal a =
  Eval.HasSubstValue IR.NoExt primTy (TypedPrim primTy primVal) a

type PrimSubstValue primTy primVal =
  ( PrimSubstValue1 primTy primVal primTy,
    PrimSubstValue1 primTy primVal (TypedPrim primTy primVal)
  )

type PrimPatSubstTerm1 primTy primVal a =
  Eval.HasPatSubstTerm (OnlyExts.T T) primTy (TypedPrim primTy primVal) a

type PrimPatSubstTerm primTy primVal =
  ( PrimPatSubstTerm1 primTy primVal primTy,
    PrimPatSubstTerm1 primTy primVal (TypedPrim primTy primVal)
  )

type CanTC' ext primTy primVal m =
  ( HasThrowTC' IR.NoExt ext primTy primVal m,
    HasGlobals primTy primVal m,
    PrimSubstValue primTy primVal,
    PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal
  )

type CanTC primTy primVal m = CanTC' IR.NoExt primTy primVal m

exec ::
  GlobalsT primTy primVal ->
  EnvTypecheck primTy primVal a ->
  (Either (TypecheckError primTy primVal) a, EnvCtx primTy primVal)
exec globals (EnvTyp env) =
  runState (runExceptT env) $ EnvCtx globals

type Context primTy primVal = [AnnotationT primTy primVal]

lookupCtx ::
  (Eval.HasWeak primTy, Eval.HasWeak primVal) =>
  Context primTy primVal ->
  IR.BoundVar ->
  Maybe (AnnotationT primTy primVal)
lookupCtx gam x = do
  Annotation π ty <- atMay gam (fromIntegral x)
  pure $ Annotation π (Eval.weakBy (x + 1) ty)

lookupGlobal ::
  (HasGlobals primTy primVal m, HasThrowTC' IR.NoExt ext primTy primVal m) =>
  IR.GlobalName ->
  m (ValueT primTy primVal, IR.GlobalUsage)
lookupGlobal x = do
  mdefn <- asks @"globals" $ HashMap.lookup x
  case mdefn of
    Just defn -> pure $ makeGAnn defn
    Nothing -> throwTC (UnboundGlobal x)
  where
    makeGAnn (IR.GDatatype (IR.Datatype {dataArgs, dataLevel})) =
      (foldr makePi (IR.VStar' dataLevel mempty) dataArgs, IR.GZero)
    makeGAnn (IR.GDataCon (IR.DataCon {conType})) =
      (conType, IR.GOmega)
    makeGAnn (IR.GFunction (IR.Function {funType, funUsage})) =
      (funType, funUsage)
    makeGAnn (IR.GAbstract (IR.Abstract {absUsage, absType})) =
      (absType, absUsage)
    makePi (IR.DataArg {argUsage, argType}) res =
      IR.VPi' argUsage argType res mempty

type UContext = [Usage.T]

type PatBinds primTy primVal = IntMap (AnnotationT primTy primVal)

type PatUsages = IntMap Usage.T

data InnerState' (ext :: Type) primTy primVal
  = InnerState
      { param :: Param.Parameterisation primTy primVal,
        patBinds :: PatBinds primTy primVal,
        bound :: Context primTy primVal
      }
  deriving (Generic)

type InnerState = InnerState' IR.NoExt

type InnerTCAlias ext primTy primVal =
  StateT (InnerState' ext primTy primVal)

newtype InnerTCT ext primTy primVal m a
  = InnerTC (InnerTCAlias ext primTy primVal m a)
  deriving newtype (Functor, Applicative, Monad)
  deriving
    ( HasSource "param" (Param.Parameterisation primTy primVal),
      HasReader "param" (Param.Parameterisation primTy primVal)
    )
    via ReaderField "param" (InnerTCAlias ext primTy primVal m)
  deriving
    ( HasSource "patBinds" (PatBinds primTy primVal),
      HasSink "patBinds" (PatBinds primTy primVal),
      HasState "patBinds" (PatBinds primTy primVal)
    )
    via StateField "patBinds" (InnerTCAlias ext primTy primVal m)
  deriving
    ( HasSource "bound" (Context primTy primVal),
      HasSink "bound" (Context primTy primVal),
      HasState "bound" (Context primTy primVal)
    )
    via StateField "bound" (InnerTCAlias ext primTy primVal m)

deriving via
  Lift (InnerTCAlias ext primTy primVal m)
  instance
    HasThrow "typecheckError" (TypecheckError' IR.NoExt ext primTy primVal) m =>
    HasThrow
      "typecheckError"
      (TypecheckError' IR.NoExt ext primTy primVal)
      (InnerTCT ext primTy primVal m)

deriving via
  Lift (InnerTCAlias ext primTy primVal m)
  instance
    HasSource "globals" (GlobalsT primTy primVal) m =>
    HasSource
      "globals"
      (GlobalsT primTy primVal)
      (InnerTCT ext primTy primVal m)

deriving via
  Lift (InnerTCAlias ext primTy primVal m)
  instance
    HasReader "globals" (GlobalsT primTy primVal) m =>
    HasReader
      "globals"
      (GlobalsT primTy primVal)
      (InnerTCT ext primTy primVal m)

type InnerTC' ext primTy primVal =
  InnerTCT ext primTy primVal (EnvTypecheck' ext primTy primVal)

type InnerTC primTy primVal = InnerTC' IR.NoExt primTy primVal

type HasParam primTy primVal =
  HasReader "param" (Param.Parameterisation primTy primVal)

type HasPatBinds primTy primVal = HasState "patBinds" (PatBinds primTy primVal)

type HasBound primTy primVal = HasState "bound" (Context primTy primVal)

type CanInnerTC' ext primTy primVal m =
  ( CanTC' ext primTy primVal m,
    HasParam primTy primVal m,
    HasPatBinds primTy primVal m,
    HasBound primTy primVal m
  )

type CanInnerTC primTy primVal m = CanInnerTC' IR.NoExt primTy primVal m

execInner ::
  Monad m =>
  InnerTCT ext primTy primVal m a ->
  InnerState' ext primTy primVal ->
  m a
execInner (InnerTC m) = evalStateT m

execInner' ::
  Monad m =>
  InnerTCT ext primTy primVal m a ->
  InnerState' ext primTy primVal ->
  m (a, InnerState' ext primTy primVal)
execInner' (InnerTC m) = runStateT m
