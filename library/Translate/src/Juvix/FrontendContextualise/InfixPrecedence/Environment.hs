{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Juvix.FrontendContextualise.InfixPrecedence.Environment
  ( module Juvix.FrontendContextualise.InfixPrecedence.Environment,
    module Juvix.FrontendContextualise.Environment,
  )
where

import Data.Kind (Constraint)
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as Local
import Juvix.FrontendContextualise.Environment
  ( HasNew,
    HasOld,
    SymbLookup (..),
    add,
    addGlobal,
    addUnknown,
    addUnknownGlobal,
    ask,
    lookup,
    lookupCurrent,
    oneFilled,
    remove,
    removeGlobal,
    removeOld,
    setupFill,
    setupNewModule,
    switchContext,
  )
import qualified Juvix.FrontendContextualise.InfixPrecedence.ShuntYard as Shunt
import qualified Juvix.FrontendContextualise.InfixPrecedence.Types as New
import qualified Juvix.FrontendContextualise.ModuleOpen.Types as Old
import Juvix.Library hiding (ask)
import qualified Juvix.Library

--------------------------------------------------------------------------------
-- Type Aliases and effect setup
--------------------------------------------------------------------------------

type Old f =
  f (NonEmpty (Old.FunctionLike Old.Expression)) Old.Signature Old.Type

type New f =
  f (NonEmpty (New.FunctionLike New.Expression)) New.Signature New.Type

type Old' f =
  f (NonEmpty (Old.FunctionLike Old.Expression)) Old.Signature

type New' f =
  f (NonEmpty (New.FunctionLike New.Expression)) New.Signature

type TransitionMap m =
  (HasState "old" (Old Context.T) m, HasState "new" (New Context.T) m)

type Queryable tag m =
  (HasReader "dispatch" tag m, QueryConstraint tag m, Query tag)

-- The effect of expression and below note that new is not the new map
-- per se, but more instead of where local functions get added...  for
-- a full pass this is indeed the new context, but for a single pass,
-- just a local cache
type Expression tag m =
  (Queryable tag m, HasState "new" (New Context.T) m, HasThrow "error" Error m, MonadIO m)

type MinimalEnv (a :: Type) (b :: Type) (c :: Type) (m :: Type -> Type) =
  (HasState "env" (Context.T a b c) m, HasState "new" (New Context.T) m)

-- the effect of the pass itself
type WorkingMaps tag m =
  (TransitionMap m, Expression tag m, HasThrow "error" Error m, MonadIO m)

-- the effect of the single function down
type SingleMap a b c m =
  (MinimalEnv a b c m, Expression (SingleDispatch a b c) m)

type FinalContext = New Context.T

--------------------------------------------------------------------------------
-- Environment data declarations
--------------------------------------------------------------------------------

-- | the traditional transition between one context and another
data EnvDispatch = EnvDispatch

-- | Used when going trying to transition one definition to the next level
data SingleDispatch (a :: Type) (b :: Type) (c :: Type) = SingleDispatch

-- | @SingleEnv@ is used when we are trying to convert one function to the next step
data SingleEnv term1 ty1 sumRep1
  = Single
      { env :: Context.T term1 ty1 sumRep1,
        temp :: New Context.T,
        disp :: SingleDispatch term1 ty1 sumRep1
      }
  deriving (Generic)

-- | @Environment@ is the environment used when going from one entire
-- phase to another
data Environment
  = Env
      { old :: Old Context.T,
        new :: New Context.T,
        dispatch :: EnvDispatch
      }
  deriving (Generic)

data Error
  = UnknownSymbol Context.NameSymbol
  | Clash
      (Shunt.Precedence Context.NameSymbol)
      (Shunt.Precedence Context.NameSymbol)
  | ImpossibleMoreEles
  | PathError Context.NameSymbol
  deriving (Show)

type ContextAlias =
  ExceptT Error (StateT Environment IO)

newtype Context a = Ctx {antiAlias :: ContextAlias a}
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving
    ( HasState "old" (Old Context.T),
      HasSink "old" (Old Context.T),
      HasSource "old" (Old Context.T)
    )
    via StateField "old" ContextAlias
  deriving
    ( HasState "new" (New Context.T),
      HasSink "new" (New Context.T),
      HasSource "new" (New Context.T)
    )
    via StateField "new" ContextAlias
  deriving
    ( HasReader "dispatch" EnvDispatch,
      HasSource "dispatch" EnvDispatch
    )
    via ReaderField "dispatch" ContextAlias
  deriving
    (HasThrow "error" Error)
    via MonadError ContextAlias

type SingleAlias term1 ty1 sumRep1 =
  ExceptT Error (StateT (SingleEnv term1 ty1 sumRep1) IO)

newtype SingleCont term1 ty1 sumRep1 a = SCtx {aSingle :: SingleAlias term1 ty1 sumRep1 a}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "new" (New Context.T),
      HasSink "new" (New Context.T),
      HasSource "new" (New Context.T)
    )
    via Rename "temp" (StateField "temp" (SingleAlias term1 ty1 sumRep1))
  deriving
    ( HasState "env" (Context.T term1 ty1 sumRep1),
      HasSink "env" (Context.T term1 ty1 sumRep1),
      HasSource "env" (Context.T term1 ty1 sumRep1)
    )
    via (StateField "env" (SingleAlias term1 ty1 sumRep1))
  deriving
    ( HasReader "dispatch" (SingleDispatch term1 ty1 sumRep1),
      HasSource "dispatch" (SingleDispatch term1 ty1 sumRep1)
    )
    via Rename "disp" (ReaderField "disp" (SingleAlias term1 ty1 sumRep1))
  deriving
    (HasThrow "error" Error)
    via MonadError (SingleAlias term1 ty1 sumRep1)

--------------------------------------------------------------------------------
-- Generic Interface Definitions for Environment lookup
--------------------------------------------------------------------------------

class Query a where
  type QueryConstraint a (m :: * -> *) :: Constraint
  queryInfo' ::
    (SymbLookup sym, QueryConstraint a m) => sym -> a -> m (Maybe [Context.Information])

instance Query (SingleDispatch a b c) where
  type QueryConstraint _ m = MinimalEnv a b c m
  queryInfo' sym SingleDispatch =
    chooseProperScope <$> lookup sym <*> (get @"env" >>| look sym)

instance Query EnvDispatch where
  type QueryConstraint _ m = TransitionMap m
  queryInfo' sym EnvDispatch =
    chooseProperScope <$> lookup sym <*> ask sym

-- dealing with the second map winning lookup
-- Main rules:
-- Local Private > Any
-- Local         > Global
-- Global        > Nothing
chooseProperScope ::
  Maybe (Context.From (Context.Definition term1 ty1 sumRep1)) ->
  Maybe (Context.From (Context.Definition term2 ty2 sumRep2)) ->
  Maybe [Context.Information]
chooseProperScope (Just (Context.Outside _)) (Just (Context.Current x)) =
  extractInformation (Local.extractValue x)
chooseProperScope Nothing (Just (Context.Current x)) =
  extractInformation (Local.extractValue x)
chooseProperScope _ (Just (Context.Current (Local.Priv x))) =
  extractInformation x
chooseProperScope Nothing (Just (Context.Outside x)) =
  extractInformation x
-- dealing with the first map winning lookup
chooseProperScope (Just x) _ =
  extractInformation (Context.extractValue x)
chooseProperScope Nothing Nothing =
  Nothing

extractInformation ::
  Context.Definition term ty sumRep -> Maybe [Context.Information]
extractInformation (Context.Def Context.D {defPrecedence}) =
  Just [Context.Prec defPrecedence]
extractInformation (Context.Information is) =
  Just is
extractInformation _ = Nothing

queryInfo ::
  (Queryable a m, SymbLookup sym) => sym -> m (Maybe [Context.Information])
queryInfo s = Juvix.Library.ask @"dispatch" >>= queryInfo' s

runEnv :: Context a -> Old Context.T -> IO (Either Error a, Environment)
runEnv (Ctx c) old = do
  ctx <- setupNewModule old
  Env old ctx EnvDispatch
    |> runStateT (runExceptT c)
