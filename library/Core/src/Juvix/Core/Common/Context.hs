-- |
-- - Serves as the context for lower level programs of the =Juvix=
--   Programming Language
-- - This is parameterized per phase which may store the type and
--   term in slightly different ways
module Juvix.Core.Common.Context
  ( module Juvix.Core.Common.Context.Types,
    module Juvix.Core.Common.Context.Precedence,
    -- leave the entire module for now, so lenses can be exported
    module Juvix.Core.Common.Context,
  )
where

import Control.Lens hiding ((|>))
import Juvix.Core.Common.Context.Precedence
import Juvix.Core.Common.Context.Types
import qualified Juvix.Core.Common.NameSpace as NameSpace
import Juvix.Library hiding (modify, toList)
import qualified Juvix.Library as Lib
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified StmContainers.Map as STM
import Prelude (error)

--------------------------------------------------------------------------------
-- In lieu of not being able to export namespaces
--------------------------------------------------------------------------------
type NameSymbol = NameSymbol.T

nameSymbolToSymbol :: NameSymbol.T -> Symbol
nameSymbolToSymbol = NameSymbol.toSymbol

nameSymbolFromSymbol :: Symbol -> NameSymbol.T
nameSymbolFromSymbol = NameSymbol.fromSymbol

--------------------------------------------------------------------------------
-- Body
--------------------------------------------------------------------------------

empty :: NameSymbol.T -> IO (T term ty sumRep)
empty sym = do
  empty <- atomically fullyEmpty
  res <- addPathWithValue (pure topLevelName <> sym') CurrentNameSpace empty
  case res of
    Lib.Left _ -> error "impossible"
    Lib.Right x -> pure x
  where
    fullyEmpty = do
      currentNameSpace <- emptyRecord
      pure $
        T
          { currentNameSpace = currentNameSpace,
            currentName = sym',
            topLevelMap = HashMap.empty,
            reverseLookup = HashMap.empty
          }
    sym' = removeTopName sym

qualifyName :: NameSymbol.T -> T term ty sumRep -> NameSymbol.T
qualifyName sym T {currentName} = currentName <> sym

emptyRecord :: STM (Record term ty sumRep)
emptyRecord = do
  emptyQualificationMap <- STM.new
  pure
    Rec
      { recordContents = NameSpace.empty,
        recordOpenList = [],
        recordQualifiedMap = emptyQualificationMap,
        recordMTy = Nothing
      }

data AmbiguousDef = AmbiguousDef NameSymbol.T

-- | @persistDefinition@ states that the definition we are adding is
-- staying in the context, so promote it.  In the future this will be
-- called for all definitions added by default, with private
-- definitions not added. However this infra is not up, so the pass
-- adding functions must add it themselves
persistDefinition ::
  T term ty sumRep -> NameSymbol.T -> Symbol -> IO (Either AmbiguousDef ())
persistDefinition T {reverseLookup} moduleName name =
  case HashMap.lookup moduleName reverseLookup of
    Just xs -> do
      determined <- determineSafe xs
      case determined of
        Lib.Left err -> pure $ Lib.Left err
        Lib.Right () -> Lib.Right <$> traverse_ f xs
      where
        -- TODO ∷ replace with a breaking foldM later
        -- this isn't in as I have to do IO actions
        determineSafe [] = pure (Lib.Right ())
        determineSafe (Who {modName, symbolMap} : xs) = do
          lookd <- atomically $ STM.lookup name symbolMap
          case lookd of
            Nothing -> determineSafe xs
            Just SymInfo {mod}
              | mod == moduleName -> determineSafe xs
              | otherwise -> pure (Lib.Left (AmbiguousDef modName))
        -- we do a check before this so the call is always safe
        f Who {symbolMap} = atomically do
          lookd <- STM.lookup name symbolMap
          case lookd of
            Nothing ->
              STM.insert (SymInfo NotUsed moduleName) name symbolMap
            -- TODO ∷ we may change behavior here based on implicit vs explicit
            Just SymInfo {} -> pure ()
    Nothing -> pure (Lib.Right ())

--------------------------------------------------------------------------------
-- Functions on the Current NameSpace
--------------------------------------------------------------------------------

lookupCurrent ::
  NameSymbol.T -> T term ty sumRep -> Maybe (NameSpace.From (Definition term ty sumRep))
lookupCurrent =
  lookupGen (\_ currentLookup -> currentLookup)

-- TODO ∷ Maybe change
-- By default add adds it to the public map by default!
add ::
  NameSpace.From Symbol ->
  Definition term ty sumRep ->
  T term ty sumRep ->
  T term ty sumRep
add sy term = over (_currentNameSpace . contents) (NameSpace.insert sy term)

remove ::
  NameSpace.From Symbol -> T term ty sumRep -> T term ty sumRep
remove sy = over (_currentNameSpace . contents) (NameSpace.remove sy)

publicNames :: T term ty sumRep -> [Symbol]
publicNames t =
  let NameSpace.List {publicL} = toList t
   in fst <$> publicL

toList :: T term ty sumRep -> NameSpace.List (Definition term ty sumRep)
toList t = NameSpace.toList (t ^. _currentNameSpace . contents)

topList :: T term ty sumRep -> [(Symbol, Definition term ty sumRep)]
topList T {topLevelMap} = HashMap.toList topLevelMap

--------------------------------------------------------------------------------
-- Global Functions
--------------------------------------------------------------------------------

-- we lose some type information here... we should probably reserve it somehow

-- | inNameSpace works like in-package in CL
-- we simply just change from our current namespace to another
inNameSpace :: NameSymbol.T -> T term ty sumRep -> Maybe (T term ty sumRep)
inNameSpace newNameSpace t@T {currentName}
  | removeTopName newNameSpace == removeTopName currentName =
    pure t
  | otherwise = do
    mdef <- t !? newNameSpace
    -- resolve the full name to be the new current module
    let (def, fullName) = resolveName t (mdef, newNameSpace)
    case def of
      Record cont ->
        Just (changeCurrentModuleWith t cont fullName)
      _ ->
        Nothing

-- | switchNameSpace works like a mixture of defpackage and in-package from CL
-- creating a namespace if not currently there, and switching to it
-- this function may fail if a path is given like `Foo.Bar.x.f` where x
-- is a non record.
-- This function also keeps the invariant that there is only one CurrentNameSpace
-- tag
switchNameSpace ::
  NameSymbol.T -> T term ty sumRep -> IO (Either PathError (T term ty sumRep))
switchNameSpace newNameSpace t@T {currentName}
  | removeTopName newNameSpace == removeTopName currentName =
    pure (Lib.Right t)
  | otherwise = do
    -- We do some repeat work here, namely we allocate an empty record
    -- in case we need to place it in for the new namespace
    empty <- atomically emptyRecord
    -- addPathWithValue will create new name spaces if they don't
    -- already exist all the way to where we need it to be
    newT <-
      do
        addPathWithValue newNameSpace (Record empty) t
        >>| \case
          Lib.Right t -> t
          Lib.Left {} -> t
    -- here we repeat work,
    -- if we successfully added the value, then we have to go down the
    -- same path to retrieve the module, hence duplicating the work
    pure $ case inNameSpace newNameSpace newT of
      Nothing -> Lib.Left (VariableShared newNameSpace)
      Just ct -> Lib.Right ct

lookup ::
  NameSymbol.T -> T term ty sumRep -> Maybe (From (Definition term ty sumRep))
lookup key t@T {topLevelMap} =
  let f x currentLookup =
        fmap Current currentLookup <|> fmap Outside (HashMap.lookup x topLevelMap)
   in lookupGen f key t

(!?) ::
  T term ty sumRep -> NameSymbol.T -> Maybe (From (Definition term ty sumRep))
(!?) = flip lookup

modifyGlobal ::
  NameSymbol.T ->
  (Maybe (Definition term ty sumRep) -> Definition term ty sumRep) ->
  T term ty sumRep ->
  T term ty sumRep
modifyGlobal sym g t =
  case modifySpace f sym t of
    Just tt -> tt
    Nothing -> t
  where
    f (Final x) =
      UpdateNow (g x)
    f (Continue (Just (Record record))) =
      GoOn record
    f (Continue _) =
      Abort

addGlobal ::
  NameSymbol.T ->
  Definition term ty sumRep ->
  T term ty sumRep ->
  T term ty sumRep
addGlobal sym def t =
  case modifySpace f sym t of
    Just tt -> tt
    Nothing -> t
  where
    f (Final _) =
      UpdateNow def
    f (Continue (Just (Record record))) =
      GoOn record
    f (Continue _) =
      Abort

addPathWithValue ::
  NameSymbol.T ->
  Definition term ty sumRep ->
  T term ty sumRep ->
  IO (Either PathError (T term ty sumRep))
addPathWithValue sym def t = do
  ret <- modifySpaceImp f sym t
  case ret of
    Just tt -> pure (Lib.Right tt)
    Nothing -> pure (Lib.Left (VariableShared sym))
  where
    f (Final Nothing) = pure (UpdateNow def)
    f (Final (Just _)) = pure Abort
    f (Continue Nothing) =
      atomically STM.new
        >>| GoOn . Rec NameSpace.empty Nothing []
    f (Continue (Just (Record record))) =
      pure (GoOn record)
    f (Continue _) =
      pure Abort

removeNameSpace :: NameSymbol -> T term ty sumRep -> T term ty sumRep
removeNameSpace sym t =
  case modifySpace f sym t of
    Just tt -> tt
    Nothing -> t
  where
    f (Final (Just _)) =
      RemoveNow
    f (Final Nothing) =
      Abort
    f (Continue (Just (Record record))) =
      GoOn record
    f (Continue Nothing) =
      Abort
    f (Continue (Just _)) =
      Abort

removeTop :: Symbol -> T term ty sumRep -> T term ty sumRep
removeTop sym t@T {topLevelMap} =
  t {topLevelMap = HashMap.delete sym topLevelMap}

------------------------------------------------------------
-- Helpers for switching the global NameSpace
------------------------------------------------------------

-- | 'changeCurrentModuleWith' moves the current name space and inserts
-- the new namespace as the top
changeCurrentModuleWith ::
  T term ty sumRep -> Record term ty sumRep -> NonEmpty Symbol -> T term ty sumRep
changeCurrentModuleWith t startingContents newCurrName =
  let queued = queueCurrentModuleBackIn t
   in t
        { currentName = removeTopName newCurrName,
          currentNameSpace = startingContents
        }
        |> queued
        |> addGlobal newCurrName CurrentNameSpace

queueCurrentModuleBackIn ::
  T term ty sumRep -> T term ty sumRep -> T term ty sumRep
queueCurrentModuleBackIn T {currentNameSpace, currentName} =
  -- we note that the currentName is a topLevel name so it
  -- gets added from the top and doesn't confuse itself with
  -- a potnentially local insertion
  addGlobal
    (addTopNameToSngle currentName)
    (Record currentNameSpace)

-- 'putCurrentModuleBackIn' adds the current name space back to the
-- environment map
-- Note that we have to have the path to the module ready and open
-- as 'addGlobal' quits if it can't find the path
-- we HAVE to remove the current module after this

putCurrentModuleBackIn :: T term ty sumRep -> T term ty sumRep
putCurrentModuleBackIn t = queueCurrentModuleBackIn t t

addTopNameToSngle :: IsString a => NonEmpty a -> NonEmpty a
addTopNameToSngle (x :| []) = topLevelName :| [x]
addTopNameToSngle xs = xs

addTopName :: (IsString a, Eq a) => NonEmpty a -> NonEmpty a
addTopName (x :| xs)
  | topLevelName == x = x :| xs
  | otherwise = topLevelName :| (x : xs)

removeTopName :: (Eq a, IsString a) => NonEmpty a -> NonEmpty a
removeTopName (top :| x : xs)
  | topLevelName == top = x :| xs
removeTopName (top :| [])
  | top == topLevelName = "" :| []
removeTopName xs = xs

-------------------------------------------------------------------------------
-- Functions on From
-------------------------------------------------------------------------------

extractValue :: From a -> a
extractValue (Outside a) = a
extractValue (Current c) = NameSpace.extractValue c

-------------------------------------------------------------------------------
-- Functions on Information
-------------------------------------------------------------------------------
precedenceOf :: Foldable t => t Information -> Maybe Precedence
precedenceOf = fmap (\(Prec p) -> p) . find f
  where
    f (Prec _) = True

-------------------------------------------------------------------------------
-- Generalized Helpers
-------------------------------------------------------------------------------

----------------------------------------
-- Types for Generalized Helpers
----------------------------------------

data Stage b
  = -- | 'Final' signifies the last symbol that we
    -- pursue in updating a structure
    Final b
  | -- | 'Continue' signifies that there are parts of
    -- the namespace that we can still continue down
    Continue b

data Return term ty sumRep
  = -- | 'GoOn' signifies that we should continue
    -- going down records
    GoOn (Record term ty sumRep)
  | -- | 'Abort' signifies that we should cancel
    -- the changes on the map and
    Abort
  | -- | 'UpdateNow' signifies that we should
    -- update the context with the current value
    UpdateNow (Definition term ty sumRep)
  | -- | 'RemoveNow' signifies that we show remove
    -- the definition at this level
    RemoveNow

----------------------------------------
-- Type Class for Genralized Helpers
----------------------------------------

-- Type class for removing repeat code
class MapSym m where
  lookup' :: Symbol -> m a -> Maybe a
  remove' :: Symbol -> m a -> m a
  insert' :: Symbol -> a -> m a -> m a

instance MapSym (HashMap.T Symbol) where
  lookup' = HashMap.lookup
  remove' = HashMap.delete
  insert' = HashMap.insert

instance MapSym NameSpace.T where
  lookup' = NameSpace.lookup
  remove' = NameSpace.removePublic
  insert' x = NameSpace.insert (NameSpace.Pub x)

newtype PrivNameSpace v = Priv {unPriv :: NameSpace.T v}

instance MapSym PrivNameSpace where
  lookup' sym = NameSpace.lookup sym . unPriv
  remove' sym = Priv . NameSpace.removePrivate sym . unPriv
  insert' sym def = Priv . NameSpace.insert (NameSpace.Priv sym) def . unPriv

modifySpace ::
  (Stage (Maybe (Definition term ty sumRep)) -> Return term ty sumRep) ->
  NameSymbol.T ->
  T term ty sumRep ->
  Maybe (T term ty sumRep)
modifySpace f symbol t = runIdentity (modifySpaceImp (Identity . f) symbol t)

-- This function dispatches to recurseImp, and serves to deal with
-- giving recurseImp the proper map to run on. this is either the
-- private local, public local, or global
modifySpaceImp ::
  Monad m =>
  ( Stage (Maybe (Definition term ty sumRep)) ->
    m (Return term ty sumRep)
  ) ->
  NameSymbol.T ->
  T term ty sumRep ->
  m (Maybe (T term ty sumRep))
modifySpaceImp f symbol@(s :| ymbol) t =
  -- check the current Module first, to properly determine which one to go down
  case NameSpace.lookupInternal s (t ^. _currentNameSpace . contents) of
    Just (NameSpace.Pub _) ->
      applyAndSetCurrent (recurseImp f (s :| ymbol))
    Just (NameSpace.Priv _) ->
      applyAndSetCurrent (fmap (fmap unPriv) . recurseImp f symbol . Priv)
    -- The first part of the name is not in the map, maybe they fully qualified the name
    Nothing ->
      case NameSymbol.takePrefixOf (t ^. _currentName) (removeTopName symbol) of
        -- currentName : Foo.Bar, symbol: TopLevel.Foo.Bar.baz
        -- currentName : Foo.Bar, symbol: Foo.Bar.baz
        -- both of these will hit this path
        Just subPath -> do
          applyAndSetCurrent (recurseImp f subPath)
        -- For this path, we have
        -- currentName : Foo.Bar, symbol: Something.Else
        Nothing ->
          -- we do one more match as we have to make sure
          -- if we are adding foo we add it to the right place
          case ymbol of
            -- This case is
            -- currentName: Foo.Bar, symbol: s
            -- so just apply on the current, we'll stop right away
            [] -> applyAndSetCurrent (recurseImp f symbol)
            -- in this case, we have more than just a single symbol
            (newS : newYmbol) ->
              -- currentName: Foo.Bar, symbol: TopLevel.First.Rest
              -- update the TopLevel on First.Rest!
              if  | s == topLevelName ->
                    applyAndSetTop (recurseImp f (newS :| newYmbol))
                  -- currentName: Foo.Bar, symbol: First.MoreStuff
                  -- just recurse on the symbol entirely
                  | otherwise -> applyAndSetTop (recurseImp f symbol)
  where
    -- dumb repeat code but idk how to handle to remove the tuple ☹
    applyAndSetTop f =
      t |> applyAndSet f (_topLevelMap, _topLevelMap)
    applyAndSetCurrent f =
      t |> applyAndSet f (_currentNameSpace . contents, _currentNameSpace . contents)

applyAndSet ::
  (Monad m, Functor f) =>
  (t -> m (f b1)) ->
  (Getting t s t, ASetter s b2 a b1) ->
  s ->
  m (f b2)
applyAndSet f (l1, l2) t = do
  ret <- f (t ^. l1)
  pure ((\r -> set l2 r t) <$> ret)

recurseImp ::
  (MapSym map, Monad m) =>
  ( Stage (Maybe (Definition term ty sumRep)) ->
    m (Return term ty sumRep)
  ) ->
  NameSymbol.T ->
  map (Definition term ty sumRep) ->
  m (Maybe (map (Definition term ty sumRep)))
recurseImp f (x :| y : xs) cont = do
  ret <- f (Continue (lookup' x cont))
  case ret of
    GoOn record -> do
      recursed <- recurseImp f (y :| xs) (record ^. contents)
      let g newRecord =
            insert' x (record |> set contents newRecord |> Record) cont
       in pure (g <$> recursed)
    Abort ->
      pure Nothing
    RemoveNow ->
      pure (Just (remove' x cont))
    UpdateNow newRecord ->
      pure (Just (insert' x newRecord cont))
recurseImp f (x :| []) cont = do
  ret <- f (Final (lookup' x cont))
  case ret of
    UpdateNow return ->
      pure (Just (insert' x return cont))
    RemoveNow ->
      pure (Just (remove' x cont))
    -- GoOn makes no sense here, so act as an Abort
    GoOn {} -> pure Nothing
    Abort -> pure Nothing

-- couldn't figure out how to fold lenses
-- once we figure out how to do a fold like
-- foldr (\x y -> x . contents . T  . y) identity brokenKey
-- replace the recursive function with that

-- TODO ∷ add something like
-- checkGlobal
--   | NameSymbol.subsetOf currentName nameSymb

-- eventually to check if we are referencing an inner module via the top
-- This will break code where you've added local

lookupGen ::
  Traversable t =>
  ( Symbol ->
    Maybe (NameSpace.From (Definition term ty sumRep)) ->
    Maybe (t (Definition term ty sumRep))
  ) ->
  NameSymbol.T ->
  T term ty sumRep ->
  Maybe (t (Definition term ty sumRep))
lookupGen extraLookup nameSymb t =
  let recurse _ Nothing =
        Nothing
      recurse [] (Just CurrentNameSpace) =
        Just (Record (t ^. _currentNameSpace))
      recurse [] x =
        x
      recurse (x : xs) (Just (Record record)) =
        recurse xs (NameSpace.lookup x (record ^. contents))
      -- This can only happen when we hit from the global
      -- a precondition is that the current module
      -- will never have a currentNamespace inside
      recurse (x : xs) (Just CurrentNameSpace) =
        recurse xs (NameSpace.lookup x (t ^. _currentNameSpace . contents))
      recurse (_ : _) _ =
        Nothing
      first (x :| xs) =
        NameSpace.lookupInternal x (t ^. _currentNameSpace . contents)
          |> second (x :| xs)
      second (x :| xs) looked =
        extraLookup x looked
          |> \case
            Just x -> traverse (recurse xs . Just) x
            Nothing -> Nothing
   in case nameSymb of
        -- we skip the local lookup if we get a top level
        top :| x : xs
          | topLevelName == top -> second (x :| xs) Nothing
        top :| []
          | topLevelName == top -> Nothing
        x :| xs -> first (x :| xs)

-- TODO/ISSUE ∷ what if we resolve a private module
-- the path and lookup doesn't make sense much
-- we need to change namesymbol to something along
-- the lines of every symbol saying if it's public
-- or private
resolveName ::
  T a b c ->
  (From (Definition a b c), NameSymbol.T) ->
  (Definition a b c, NameSymbol.T)
resolveName ctx (def, name) =
  case def of
    -- TODO ∷ update this case
    Current (NameSpace.Priv x) ->
      (x, fullyQualified)
    Current (NameSpace.Pub x) ->
      (x, fullyQualified)
    Outside x ->
      (x, nameAlreadyFully)
  where
    nameAlreadyFully =
      pure topLevelName <> removeTopName name
    fullyQualified =
      pure topLevelName <> currentName ctx <> name

-- | qualifyLookup fully qualiifes a name in the current context.
qualifyLookup :: NameSymbol.T -> T a b c -> Maybe NameSymbol.T
qualifyLookup name ctx =
  case lookup name ctx of
    Nothing -> Nothing
    Just (Outside _) -> Just (NameSymbol.cons topLevelName (removeTopName name))
    Just (Current _) -> Just (pure topLevelName <> currentName ctx <> name)
