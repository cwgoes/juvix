{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Juvix.FrontendContextualise.ModuleOpen.Environment
  ( module Juvix.FrontendContextualise.ModuleOpen.Environment,
    module Juvix.FrontendContextualise.Environment,
    Open (..),
  )
where

import Control.Lens hiding (Context, (|>))
import qualified Data.HashSet as Set
import Data.Kind (Constraint)
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.Desugar.Types as Old
import Juvix.FrontendContextualise.Contextify.ResolveOpenInfo (Open (..))
import qualified Juvix.FrontendContextualise.Contextify.ResolveOpenInfo as ResolveOpen
import Juvix.FrontendContextualise.Environment
import qualified Juvix.FrontendContextualise.ModuleOpen.Types as New
import Juvix.Library
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified StmContainers.Map as STM

--------------------------------------------------------------------------------
-- Type Aliases and effect setup
--------------------------------------------------------------------------------

type ModuleMap = Map.T Symbol NameSymbol.T

type OpenClosure =
  Map.T Symbol NameSymbol.T

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

type SingleMap a b c m =
  (HasState "env" (Context.T a b c) m, HasState "new" (New Context.T) m)

type WorkingMaps m =
  (TransitionMap m, Expression EnvDispatch m)

type Qualification tag m =
  (CurrentModuleConstraint tag m, HasReader "dispatch" tag m, QualificationMap tag)

type ModuleSwitch tag m =
  (SwitchConstraint tag m, HasReader "dispatch" tag m, Switch tag)

type Exclusion m =
  HasState "exclusion" ExclusionSet m

type Closure m =
  HasState "closure" OpenClosure m

type SymbolQualification tag m =
  (MonadIO m, Exclusion m, Qualification tag m, Closure m)

-- The effect of expression and below note that new is not the new map
-- per se, but more instead of where local functions get added...  for
-- a full pass this is indeed the new context, but for a single pass,
-- just a local cache
type Expression tag m =
  ( HasState "new" (New Context.T) m,
    HasThrow "error" Error m,
    ModuleSwitch tag m,
    SymbolQualification tag m
  )

--------------------------------------------------------------------------------
-- Environment data declarations
--------------------------------------------------------------------------------

type ExclusionSet = Set.HashSet Symbol

-- | the traditional transition between one context and another
data EnvDispatch = EnvDispatch deriving (Show)

-- | Used when going trying to transition one definition to the next level
data SingleDispatch (a :: Type) (b :: Type) (c :: Type) = SingleDispatch

-- | @SingleEnv@ is used when we are trying to convert one function to the next step
data SingleEnv term1 ty1 sumRep1
  = Single
      { env :: Context.T term1 ty1 sumRep1,
        temp :: New Context.T,
        disp :: SingleDispatch term1 ty1 sumRep1,
        exclusionSet :: ExclusionSet,
        openClosure :: OpenClosure
      }
  deriving (Generic)

-- | @Environment@ is the environment used when going from one entire
-- phase to another
data Environment
  = Env
      { old :: Old Context.T,
        new :: New Context.T,
        dispatch :: EnvDispatch,
        exclusion :: ExclusionSet,
        closure :: OpenClosure
      }
  deriving (Generic, Show)

data Error
  = UnknownModule NameSymbol.T
  | AmbiguousSymbol Symbol
  | OpenNonModule (Set.HashSet Context.NameSymbol)
  | IllegalModuleSwitch Context.NameSymbol
  | ConflictingSymbols Context.NameSymbol
  | CantResolveModules [NameSymbol.T]
  | ModuleConflict Symbol [NameSymbol.T]
  deriving (Show, Eq)

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
    (HasThrow "error" Error)
    via MonadError ContextAlias
  deriving
    ( HasReader "dispatch" EnvDispatch,
      HasSource "dispatch" EnvDispatch
    )
    via ReaderField "dispatch" ContextAlias
  deriving
    ( HasState "exclusion" ExclusionSet,
      HasSink "exclusion" ExclusionSet,
      HasSource "exclusion" ExclusionSet
    )
    via StateField "exclusion" ContextAlias
  deriving
    ( HasState "closure" OpenClosure,
      HasSink "closure" OpenClosure,
      HasSource "closure" OpenClosure
    )
    via StateField "closure" ContextAlias

type SingleAlias term1 ty1 sumRep1 =
  ExceptT Error (StateT (SingleEnv term1 ty1 sumRep1) IO)

newtype SingleCont term1 ty1 sumRep1 a = SCtx {aSingle :: SingleAlias term1 ty1 sumRep1 a}
  deriving (Functor, Applicative, Monad, MonadIO)
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
  deriving
    ( HasState "exclusion" ExclusionSet,
      HasSink "exclusion" ExclusionSet,
      HasSource "exclusion" ExclusionSet
    )
    via Rename "exclusionSet" (StateField "exclusionSet" (SingleAlias term1 ty1 sumRep1))
  deriving
    ( HasState "closure" OpenClosure,
      HasSink "closure" OpenClosure,
      HasSource "closure" OpenClosure
    )
    via Rename "openClosure" (StateField "openClosure" (SingleAlias term1 ty1 sumRep1))

--------------------------------------------------------------------------------
-- Generic Interface Definitions for Environment lookup
--------------------------------------------------------------------------------

class QualificationMap a where
  type CurrentModuleConstraint a (m :: * -> *) :: Constraint
  symbolMap :: CurrentModuleConstraint a m => a -> m Context.SymbolMap

class Switch a where
  type SwitchConstraint a (m :: * -> *) :: Constraint
  switch :: SwitchConstraint a m => Context.NameSymbol -> a -> m ()

instance Switch (SingleDispatch a b c) where
  type
    SwitchConstraint _ m =
      (SingleMap a b c m, HasThrow "error" Error m, MonadIO m)
  switch sym SingleDispatch = do
    tmp <- get @"new"
    env <- get @"env"
    switched <- liftIO $ switchContext sym tmp env
    case switched of
      Right (t, e) -> put @"new" t >> put @"env" e
      Left _ -> throw @"error" (UnknownModule sym)

instance Switch EnvDispatch where
  type
    SwitchConstraint _ m =
      (TransitionMap m, HasThrow "error" Error m, MonadIO m)
  switch sym EnvDispatch = do
    old <- get @"old"
    new <- get @"new"
    switched <- liftIO $ switchContext sym old new
    case switched of
      Right (o, n) -> put @"old" o >> put @"new" n
      Left _ -> throw @"error" (UnknownModule sym)

instance QualificationMap EnvDispatch where
  type
    CurrentModuleConstraint _ m =
      TransitionMap m
  symbolMap EnvDispatch =
    gets @"new" (\x -> x ^. Context._currentNameSpace . Context.qualifiedMap)

instance QualificationMap (SingleDispatch a b c) where
  type
    CurrentModuleConstraint _ m =
      SingleMap a b c m
  symbolMap SingleDispatch =
    gets @"env" (\x -> x ^. Context._currentNameSpace . Context.qualifiedMap)

switchNameSpace :: ModuleSwitch tag m => NameSymbol.T -> m ()
switchNameSpace name = Juvix.Library.ask @"dispatch" >>= switch name

getSymbolMap :: Qualification tag m => m Context.SymbolMap
getSymbolMap = Juvix.Library.ask @"dispatch" >>= symbolMap

----------------------------------------
-- Helper functions for the instances
----------------------------------------

switchContextErr ::
  (HasThrow "error" Error m, MonadIO m) =>
  NameSymbol.T ->
  Context.T term ty sumRep ->
  m (Context.T term ty sumRep)
switchContextErr sym ctx = do
  switched <- liftIO $ Context.switchNameSpace sym ctx
  case switched of
    -- bad Error for now
    Left ____ -> throw @"error" (UnknownModule sym)
    Right map -> pure map

grabList :: NameSymbol.T -> Context.T a b c -> NameSpace.List (Context.Definition a b c)
grabList name ctx =
  case Context.extractValue <$> Context.lookup name ctx of
    Just (Context.Record Context.Rec {recordContents}) ->
      NameSpace.toList recordContents
    Just _ ->
      NameSpace.List [] []
    Nothing ->
      NameSpace.List [] []

combineLists :: NameSpace.List b1 -> NameSpace.List b2 -> [Symbol]
combineLists NameSpace.List {publicL = pub1} NameSpace.List {publicL = pub2} =
  fmap fst pub1 <> fmap fst pub2

inMap :: NameSymbol.T -> Context.T term ty sumRep -> Bool
inMap name ctx = isJust (Context.lookup name ctx)

type FinalContext = New Context.T

--------------------------------------------------------------------------------
-- Running functions
--------------------------------------------------------------------------------

bareRun ::
  Context a ->
  Old Context.T ->
  New Context.T ->
  ResolveOpen.OpenMap ->
  IO (Either Error a, Environment)
bareRun (Ctx c) old new _opens =
  Env old new EnvDispatch mempty mempty
    |> runStateT (runExceptT c)

runEnv ::
  Context a -> Old Context.T -> [ResolveOpen.PreQualified] -> IO (Either Error a, Environment)
runEnv (Ctx c) old pres = do
  resolved <- ResolveOpen.run old pres
  empt <- setupNewModule old
  case resolved of
    Right res ->
      Env res empt EnvDispatch mempty mempty
        |> runStateT (runExceptT c)
    Left err -> pure (Left (resolveErrToErr err), undefined)

data QualifySymbolAns
  = Local NameSymbol.T
  | GlobalInfo Context.SymbolInfo
  deriving (Show)

qualifySymbolInfo ::
  SymbolQualification tag m => NonEmpty Symbol -> m (Maybe QualifySymbolAns)
qualifySymbolInfo (s :| _) = do
  exclusion <- get @"exclusion"
  openClosure <- get @"closure"
  case Set.member s exclusion of
    True -> pure Nothing
    False ->
      case openClosure Map.!? s of
        Just nameSym ->
          pure (Just (Local nameSym))
        Nothing -> do
          qualified <- getSymbolMap
          looked <- liftIO $ atomically $ STM.lookup s qualified
          case looked of
            Just sy -> pure $ Just (GlobalInfo sy)
            Nothing -> pure Nothing

-- for this function just the first part of the symbol is enough

qualifyName ::
  SymbolQualification tag m => NonEmpty Symbol -> m (NonEmpty Symbol)
qualifyName sym = do
  mSym <- qualifySymbolInfo sym
  case mSym of
    Just (GlobalInfo Context.SymInfo {mod}) ->
      pure $ Context.addTopName (mod <> sym)
    Just (Local nameSym) ->
      pure $ Context.addTopName (nameSym <> sym)
    Nothing ->
      pure sym

-- currently unused as we don't save the function name
markUsed ::
  SymbolQualification tag m => NonEmpty Symbol -> Symbol -> m ()
markUsed sym fname = do
  mSym <- qualifySymbolInfo sym
  case mSym of
    Just (GlobalInfo sym@Context.SymInfo {used}) -> do
      qualified <- getSymbolMap
      STM.insert
        ( sym
            { Context.used = case used of
                Context.Func xs ->
                  Context.Func (fname : xs)
                Context.NotUsed ->
                  Context.Func [fname]
                Context.Yes ->
                  Context.Func [fname]
            }
        )
        fname
        qualified
        |> atomically
        |> liftIO
    Nothing -> pure ()
    Just Local {} -> pure ()

addExcludedElement :: Exclusion m => Symbol -> m ()
addExcludedElement sym =
  Juvix.Library.modify @"exclusion" (Set.insert sym)

removeExcludedElement :: Exclusion m => Symbol -> m ()
removeExcludedElement sym =
  Juvix.Library.modify @"exclusion" (Set.delete sym)

addOpen :: Closure m => Symbol -> NameSymbol.T -> m ()
addOpen sym mod = modify @"closure" (Map.insert sym mod)

removeOpen :: Closure m => Symbol -> m ()
removeOpen sym = modify @"closure" (Map.delete sym)

resolveErrToErr :: ResolveOpen.Error -> Error
resolveErrToErr rsv =
  case rsv of
    ResolveOpen.UnknownModule x -> UnknownModule x
    ResolveOpen.CantResolveModules xs -> CantResolveModules xs
    ResolveOpen.OpenNonModule hs -> OpenNonModule hs
    ResolveOpen.AmbiguousSymbol s -> AmbiguousSymbol s
    ResolveOpen.ModuleConflict s ns -> ModuleConflict s ns
    ResolveOpen.IllegalModuleSwitch ns -> IllegalModuleSwitch ns
