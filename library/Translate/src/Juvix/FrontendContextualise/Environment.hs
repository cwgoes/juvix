module Juvix.FrontendContextualise.Environment where

import Control.Lens hiding ((|>))
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.Core.Common.Open as Open
import Juvix.Library
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp
import Prelude (error)

type HasNew t ty s m = HasState "new" (Context.T t ty s) m

type HasOld t ty s m = HasState "old" (Context.T t ty s) m

-- to avoid casting all the time

class SymbLookup a where
  look ::
    a ->
    Context.T term ty sumRep ->
    Maybe (Context.From (Context.Definition term ty sumRep))

  lookCurr ::
    a ->
    Context.T term ty sumRep ->
    Maybe (NameSpace.From (Context.Definition term ty sumRep))

instance SymbLookup Symbol where
  look sym cont = Context.lookup (NameSymbol.fromSymbol sym) cont

  --
  lookCurr sym cont = Context.lookupCurrent (NameSymbol.fromSymbol sym) cont

instance SymbLookup NameSymbol.T where
  look sym cont = Context.lookup sym cont

  lookCurr sym cont = Context.lookupCurrent sym cont

lookup ::
  (HasNew term ty sumRep m, SymbLookup sym) =>
  sym ->
  m (Maybe (Context.From (Context.Definition term ty sumRep)))
lookup sy = do
  get @"new" >>| look sy

lookupCurrent ::
  (HasNew term ty sumRep m, SymbLookup sym) =>
  sym ->
  m (Maybe (NameSpace.From (Context.Definition term ty sumRep)))
lookupCurrent sy = do
  get @"new" >>| lookCurr sy

ask ::
  (HasOld term ty sumRep m, SymbLookup sym) =>
  sym ->
  m (Maybe (Context.From (Context.Definition term ty sumRep)))
ask sy = do
  get @"old" >>| look sy

add ::
  HasNew term ty sumRep m =>
  NameSpace.From Symbol ->
  Context.Definition term ty sumRep ->
  m ()
add sy def = Juvix.Library.modify @"new" (Context.add sy def)

addGlobal ::
  HasNew term ty sumRep m =>
  NameSymbol.T ->
  Context.Definition term ty sumRep ->
  m ()
addGlobal sy def = Juvix.Library.modify @"new" (Context.addGlobal sy def)

remove ::
  HasNew term ty sumRep m => NameSpace.From Symbol -> m ()
remove sy = Juvix.Library.modify @"new" (Context.remove sy)

removeGlobal ::
  HasNew term ty sumRep m => NameSymbol.T -> m ()
removeGlobal sy = Juvix.Library.modify @"new" (Context.removeNameSpace sy)

removeOld ::
  HasOld term ty sumRep m => NameSpace.From Symbol -> m ()
removeOld sy = Juvix.Library.modify @"old" (Context.remove sy)

addUnknown ::
  HasNew term ty sumRep m => NameSpace.From Symbol -> m ()
addUnknown sym =
  Juvix.Library.modify @"new"
    (Context.add sym (Context.Unknown Nothing))

addUnknownGlobal ::
  HasNew term ty sumRep m => Context.From Symbol -> m ()
addUnknownGlobal (Context.Current sym) = addUnknown sym
addUnknownGlobal (Context.Outside sym) =
  Juvix.Library.modify @"new"
    (Context.addGlobal (pure sym) (Context.Unknown Nothing))

------------------------------------------------------------
-- double module setup and dealings
------------------------------------------------------------

setupNewModule ::
  Context.T term1 ty1 sumRep1 -> IO (Context.T term2 ty2 sumRep2)
setupNewModule t = do
  empt <- Context.empty (Context.currentName t)
  empt
    |> set
      (Context._currentNameSpace . Context.qualifiedMap)
      (t ^. Context._currentNameSpace . Context.qualifiedMap)
    |> set Context._reverseLookup (t ^. Context._reverseLookup)
    |> pure

-- | @switchContext@ takes two modules representing the same context
-- and attempts to switch the namespace while keeping the pointers
-- consistent
switchContext ::
  NameSymbol.T ->
  Context.T term ty sumRep ->
  Context.T term2 ty2 sumRep2 ->
  IO
    ( Either
        Context.PathError
        (Context.T term ty sumRep, Context.T term2 ty2 sumRep2)
    )
switchContext sym ctx1 ctx2 = do
  let switched1 = Context.inNameSpace sym ctx1
      switched2 = Context.inNameSpace sym ctx2
  case (switched1, switched2) of
    (Just c1, Just c2) -> pure $ Right (c1, c2)
    (Nothing, Nothing) -> pure $ Left (Context.VariableShared sym)
    (Just c1, Nothing) -> setupFill sym c1 ctx2
    (Nothing, Just c2) -> setupFill sym c2 ctx1 >>| fmap swap

-- | @oneFilled@ takes a namesymbol and filled out parts of a currently
-- added module and creates a new context with that module filled out
oneFilled ::
  NameSymbol.T ->
  ([Open.TName NameSymbol.T], Context.SymbolMap) ->
  Context.T term ty sumRep ->
  IO (Either Context.PathError (Context.T term ty sumRep))
oneFilled sym (openList, qualifiedMap) ctx =
  Context.addPathWithValue
    sym
    (Context.Record (Context.Rec NameSpace.empty Nothing openList qualifiedMap))
    ctx

-- | @setupFilled@ takes two contexts, one that successfully switched
-- modules and another that hasn't and inserts the needed information
-- to keep the pointers consistent
setupFill ::
  NameSymbol.T ->
  Context.T term1 ty1 sumRep1 ->
  Context.T term2 ty2 sumRep2 ->
  IO
    ( Either
        Context.PathError
        (Context.T term1 ty1 sumRep1, Context.T term2 ty2 sumRep2)
    )
setupFill sym cWorks cDoesntWork = do
  let args =
        ( cWorks ^. Context._currentNameSpace . Context.openList,
          cWorks ^. Context._currentNameSpace . Context.qualifiedMap
        )
  newInserted <- oneFilled sym args cDoesntWork
  case newInserted of
    Right cInserted ->
      -- This will always be a just!
      let Just cNowWorks = Context.inNameSpace sym cInserted
       in pure $ Right (cWorks, cNowWorks)
    Left err -> pure $ Left err

----------------------------------------------------------------------
-- Sexp Helpers Code Above this Will likely be deleted
----------------------------------------------------------------------

-- TODO ∷ make this a standard data structure

-- Currently we don't really use the signature however in the future
-- the mSig will be used to detect the types of modules we will have
-- open and any other information we wish to track here!?
data Information
  = Info
      { -- | @mSig@ represents the type of the term in the closure
        mSig :: Maybe Sexp.T,
        -- | @info@ represents all the information we have on the term
        info :: [Context.Information],
        -- | @mOpen@ represents a place where the term may have come
        -- from
        mOpen :: Maybe NameSymbol.T
      }
  deriving (Show, Eq)

newtype Closure'
  = Closure (Map.T Symbol Information)
  deriving (Show, Eq)

data ErrorS = CantResolve [Sexp.T]

type SexpContext = Context.T Sexp.T Sexp.T Sexp.T

type HasClosure m = HasReader "closure" Closure' m

type ContextS m = HasState "context" SexpContext m

type ErrS m = HasThrow "error" ErrorS m

addToClosure :: Symbol -> Information -> Closure' -> Closure'
addToClosure k info (Closure m) =
  Closure $ Map.insert k info m

genericBind :: Symbol -> Closure' -> Closure'
genericBind name (Closure m) =
  Closure $ Map.insert name (Info Nothing [] Nothing) m

passContext ctx f g h =
  Context.mapWithContext
    ctx
    Context.CtxForm
      { sumF = f,
        termF = g,
        tyF = h
      }
  where
    pass =
      undefined

bindingForms :: (Eq a, IsString a) => a -> Bool
bindingForms x =
  x `elem` ["type", ":open-in", ":let-type", ":let-match", "case", ":lambda-case", ":declaim"]

searchAndClosure ::
  (HasClosure f, ErrS f) => SexpContext -> Sexp.Atom -> Sexp.T -> (Sexp.T -> f Sexp.T) -> f Sexp.T
searchAndClosure ctx a as cont
  | named "case" = case' as cont
  -- this case happens at the start of every defun
  | named ":lambda-case" = lambdaCase as cont
  -- This case is a bit special, as we must check the context for
  -- various names this may introduce to the
  | named ":open-in" = openIn ctx as cont
  | named ":declaim" = undefined
  where
    named = Sexp.isAtomNamed (Sexp.Atom a)

openIn ::
  (ErrS f, HasClosure f) => SexpContext -> Sexp.T -> (Sexp.T -> f Sexp.T) -> f Sexp.T
openIn ctx (Sexp.List [mod, body]) cont = do
  -- Fully run what we need to on mod
  newMod <- cont mod
  -- Now let us open up the box
  case Sexp.atomFromT newMod of
    Just Sexp.A {atomName} ->
      case ctx Context.!? atomName >>| Context.extractValue of
        Just (Context.Record record) ->
          let NameSpace.List {publicL} = NameSpace.toList (record ^. Context.contents)
              --
              newSymbs = fst <$> publicL
              --
              addSymbolInfo symbol =
                addToClosure symbol (Info Nothing [] (Just atomName))
           in --
              local @"closure" (\cnt -> foldr addSymbolInfo cnt newSymbs) do
                newBody <- cont body
                pure $ Sexp.list [newMod, newBody]
        _ ->
          throw @"error" (CantResolve [newMod])
    _ ->
      throw @"error" (CantResolve [newMod])

lambdaCase :: HasClosure f => Sexp.T -> (Sexp.T -> f Sexp.T) -> f Sexp.T
lambdaCase binds cont =
  mapF (`matchMany` cont) binds

case' :: HasClosure f => Sexp.T -> (Sexp.T -> f Sexp.T) -> f Sexp.T
case' (t Sexp.:> binds) cont = do
  op <- cont t
  binding <- mapF (`match` cont) binds
  pure (Sexp.Cons op binding)
case' _ _ = error "malformed case"

matchMany :: HasClosure m => Sexp.T -> (Sexp.T -> m Sexp.T) -> m Sexp.T
matchMany = matchGen nameStar

match :: HasClosure m => Sexp.T -> (Sexp.T -> m Sexp.T) -> m Sexp.T
match = matchGen nameStarSingle

matchGen ::
  (HasClosure m, Foldable t) =>
  (Sexp.T -> t Symbol) ->
  Sexp.T ->
  (Sexp.T -> m Sexp.T) ->
  m Sexp.T
matchGen nameStarFunc (Sexp.List [path, body]) cont =
  -- Important we must do this first!
  local @"closure" (\cnt -> foldr genericBind cnt grabBindings) $ do
    -- THIS MUST happen in the local, as we don't want to have a pas
    -- confuse the variables here as something else... imagine if we
    -- are doing a pass which resolves symbols, then we'd try to
    -- resolve the variables we bind. However for constructors and what
    -- not they need to be ran through this pass
    newPath <- cont path
    newB <- cont body
    pure (Sexp.list [newPath, newB])
  where
    grabBindings = nameStarFunc path
matchGen _ _ _ = error "malformed match"

-- | @nameStarSingle@ like @nameStar@ but we are matching on a single
-- element
nameStarSingle :: Sexp.T -> [Symbol]
nameStarSingle = nameStar . (\x -> Sexp.list [x])

-- | @nameStar@ grabs names recursively
nameStar :: Sexp.T -> [Symbol]
nameStar ((_caar Sexp.:> cadr) Sexp.:> cdr) =
  -- we ignore the _caar as it's a cosntructor!
  nameStar cadr <> nameStar cdr
nameStar (x Sexp.:> xs)
  | Just symb <- eleToSymbol x =
    symb : nameStar xs
  | otherwise =
    -- the car is not a cons or an atom, thus a number, we should
    -- ignore it
    nameStar xs
nameStar Sexp.Atom {} = []
nameStar Sexp.Nil = []

------------------------------------------------------------
-- Move to Sexp library
------------------------------------------------------------

mapF :: Applicative f => (Sexp.T -> f Sexp.T) -> Sexp.T -> f Sexp.T
mapF f (x Sexp.:> xs) =
  Sexp.Cons <$> f x <*> mapF f xs
mapF _ Sexp.Nil = pure Sexp.Nil
mapF _ a = pure a

eleToSymbol :: Sexp.T -> Maybe Symbol
eleToSymbol x
  | Just Sexp.A {atomName} <- Sexp.atomFromT x =
    Just (NameSymbol.toSymbol atomName)
  | otherwise = Nothing
