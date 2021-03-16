{-# LANGUAGE LiberalTypeSynonyms #-}

-- |
-- - order of Passes
--   1. =ModuleOpen=
--   2. =InfixPrecedence=
module Juvix.FrontendContextualise
  ( module Juvix.FrontendContextualise,
    Target.FinalContext,
  )
where

import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Desugar.Types as Initial
import qualified Juvix.FrontendContextualise.Contextify.ResolveOpenInfo as ResolveOpen
import qualified Juvix.FrontendContextualise.Contextify.Sexp as ContextSexp
import qualified Juvix.FrontendContextualise.Contextify.Transform as Contextify
import qualified Juvix.FrontendContextualise.Contextify.Types as Contextify
import qualified Juvix.FrontendContextualise.InfixPrecedence.Environment as Infix
import qualified Juvix.FrontendContextualise.InfixPrecedence.Environment as Target
import qualified Juvix.FrontendContextualise.InfixPrecedence.Transform as Infix
import qualified Juvix.FrontendContextualise.ModuleOpen.Environment as Module
import qualified Juvix.FrontendContextualise.ModuleOpen.Transform as Module
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp

data Error
  = ModuleErr Module.Error
  | InfixErr Infix.Error
  | PathErr Context.PathError
  deriving (Show)

type Final f = Target.New f

type Final' f = Target.New' f

op ::
  NonEmpty (NameSymbol.T, [Initial.TopLevel]) -> IO (Either Error Target.FinalContext)
op = contextualize

contextualize ::
  NonEmpty (NameSymbol.T, [Initial.TopLevel]) -> IO (Either Error Target.FinalContext)
contextualize init = do
  ctx <- contextify init
  case ctx of
    Left err -> pure $ Left (PathErr err)
    Right (context, openList) -> do
      trans1 <- Module.transformContext context openList
      case trans1 of
        Left err -> pure $ Left (ModuleErr err)
        Right xs -> do
          infixed <- Infix.transformContext xs
          case infixed of
            Left err -> pure $ Left (InfixErr err)
            Right xs -> pure $ Right xs

contextify ::
  NonEmpty (NameSymbol.T, [Initial.TopLevel]) ->
  IO (Either Context.PathError (Contextify.Context, [ResolveOpen.PreQualified]))
contextify t@((sym, _) :| _) = do
  emptyCtx <- Context.empty sym
  runM $
    foldM resolveOpens (emptyCtx, []) (addTop <$> t)

addTop :: Bifunctor p => p NameSymbol.T c -> p NameSymbol.T c
addTop = first (NameSymbol.cons Context.topLevelName)

-- we get the opens
resolveOpens ::
  (MonadIO m, HasThrow "left" Context.PathError m) =>
  (Contextify.Context, [ResolveOpen.PreQualified]) ->
  (Context.NameSymbol, [Initial.TopLevel]) ->
  m (Contextify.Context, [ResolveOpen.PreQualified])
resolveOpens (ctx', openList) (sym, xs) = do
  ctx <- liftIO (Contextify.run ctx' (sym, xs))
  case ctx of
    Right Contextify.P {ctx, opens, modsDefined} ->
      pure
        ( ctx,
          ResolveOpen.Pre
            { opens,
              explicitModule = sym,
              implicitInner = modsDefined
            }
            : openList
        )
    Left err -> throw @"left" err

type RunM =
  ExceptT Context.PathError IO

newtype M a = M (RunM a)
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving (HasThrow "left" Context.PathError) via MonadError RunM

runM :: M a -> IO (Either Context.PathError a)
runM (M a) = runExceptT a

------------------------------------------------------------
-- S-expression Version
------------------------------------------------------------

contextifyS ::
  NonEmpty (NameSymbol.T, [Sexp.T]) ->
  IO
    ( Either
        Context.PathError
        (Contextify.ContextSexp, [ResolveOpen.PreQualified])
    )
contextifyS t@((sym, _) :| _) = do
  emptyCtx <- Context.empty sym
  runM $
    foldM resolveOpensS (emptyCtx, []) (addTop <$> t)

addTopS :: Bifunctor p => p NameSymbol.T c -> p NameSymbol.T c
addTopS = first (NameSymbol.cons Context.topLevelName)

-- we get the opens
resolveOpensS ::
  (MonadIO m, HasThrow "left" Context.PathError m) =>
  (Contextify.ContextSexp, [ResolveOpen.PreQualified]) ->
  (Context.NameSymbol, [Sexp.T]) ->
  m (Contextify.ContextSexp, [ResolveOpen.PreQualified])
resolveOpensS (ctx', openList) (sym, xs) = do
  ctx <- ContextSexp.run ctx' (sym, xs)
  case ctx of
    Right Contextify.PS {ctxS, opensS, modsDefinedS} ->
      pure
        ( ctxS,
          ResolveOpen.Pre
            { opens = opensS,
              explicitModule = sym,
              implicitInner = modsDefinedS
            }
            : openList
        )
    Left err -> throw @"left" err
