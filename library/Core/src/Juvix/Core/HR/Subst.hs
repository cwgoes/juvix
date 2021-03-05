-- |
-- - Runs a substitution algorithm over core
module Juvix.Core.HR.Subst where

import Control.Lens hiding (op, (|>))
import qualified Data.HashSet as Set
import qualified Juvix.Core.HR.Types as Types
import Juvix.Library
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

-- eventually a lot of this code will be replaced by a proper
-- inliner strategy, but this code will serve as the basis for
-- said iterative design

data Key
  = Bound Natural
  | Global NameSymbol.T
  deriving (Show, Eq, Generic)

instance Hashable Key

data T primTy primVal
  = T
      { _seenSet :: InScopeSet,
        _sub :: Map.T NameSymbol.T (Types.Elim primTy primVal)
      }
  deriving (Show)

type InScopeSet = Set.HashSet NameSymbol.T

makeLenses ''T

-- TODO ∷
-- - add a context for this to run with
-- - Turn the Context, T, InScopeSet to being in the Env
-- - when seeing a new binding determine if we should add that to the Τ type
--   + For now we just ignore extra values to inline, and continue on our way
op ::
  T primTy primVal -> Types.Term primTy primVal -> Types.Term primTy primVal
op _ (Types.PrimTy ty) =
  Types.PrimTy ty
op _ (Types.Star uni) =
  Types.Star uni
op subst (Types.Pi usage name typ body) =
  let (newSubst, newName) = uniqueNameAndUpdateMap subst name
   in Types.Pi usage newName typ (op newSubst body)
op subst (Types.Lam name body) =
  let (newSubst, newName) = uniqueNameAndUpdateMap subst name
   in Types.Lam newName (op newSubst body)
op subst (Types.Sig usage name typ body) =
  let (newSubst, newName) = uniqueNameAndUpdateMap subst name
   in Types.Sig usage newName typ (op newSubst body)
op subst (Types.Pair left right) =
  Types.Pair (op subst left) (op subst right)
op subst (Types.Let usage name bound body)
  -- lets are non recrusive!
  | inlineP usage bound =
    let newBound =
          substElim subst bound
        newSubst =
          subst
            |> over sub (Map.insert name newBound)
            |> over seenSet (Set.insert name)
     in op newSubst body
  | otherwise =
    let (newSubst, newName) = uniqueNameAndUpdateMap subst name
     in Types.Let usage newName (substElim subst bound) (op newSubst body)
op _ (Types.Prim prim) =
  Types.Prim prim
op _ Types.UnitTy =
  Types.UnitTy
op _ Types.Unit =
  Types.Unit
op subst (Types.Elim elim) =
  Types.Elim (substElim subst elim)

substElim ::
  T primTy primVal -> Types.Elim primTy primVal -> Types.Elim primTy primVal
substElim subst (Types.Var v) =
  case (subst ^. sub) Map.!? v of
    -- we only do pre-inlining currently...
    -- need to think how to do post inline techniques
    Just el -> el
    Nothing -> Types.Var v
substElim subst (Types.App fun arg) =
  Types.App (substElim subst fun) (op subst arg)
substElim subst (Types.Ann usage ann term uni) =
  Types.Ann usage (op subst ann) (op subst term) uni

-- TODO ∷
-- - take an environment of some kind
--   + This env should have meta data at some point to better make
--     decisions. Or better yet tag it to the structure itself in some
--     extension.
-- - Make this run on some type of size and usage measurement.
--   + The current metric just checks if the usage is one. However
--     this is bad, as we could move the evaluation in the middle of
--     some computation that degrades performance.

-- | 'inlineP' determines if a function should be inlined
inlineP :: Usage.T -> Types.Elim primTy primVal -> Bool
inlineP usage _
  | usage == one = True
  | otherwise = False

empty :: T primTy primVal
empty = T mempty mempty

-- | 'uniqueNameAndUpdateMap' takes a Subst data structure and a symbol
-- and updates the map to redirect symbols labeled x if it already seen
uniqueNameAndUpdateMap ::
  T primTy primVal ->
  NameSymbol.T ->
  (T primTy primVal, NameSymbol.T)
uniqueNameAndUpdateMap subst sym
  | sym == newSym =
    (newSubst |> over sub (Map.delete sym), newSym)
  | otherwise =
    (newSubst |> over sub (Map.insert sym (Types.Var newSym)), newSym)
  where
    (newSet, newSym) =
      uniqueName sym (subst ^. seenSet)
    newSubst =
      subst |> set seenSet newSet

-- TODO ∷ make a unique name when the hash value is the same name
-- as a value in the hash itself... this shouldn't happen, but
-- we need the linear add one strategy when this does occur

-- | 'uniqueName' generates a unique name based off the symbol
-- and the set
uniqueName :: NameSymbol.T -> InScopeSet -> (InScopeSet, NameSymbol.T)
uniqueName sym set
  | Set.member sym set =
    (Set.insert newSym set, newSym)
  | otherwise =
    (Set.insert sym set, sym)
  where
    newSym = NameSymbol.applyBase (<> fingerprint) sym
    fingerprint = intern (show (hash set))
