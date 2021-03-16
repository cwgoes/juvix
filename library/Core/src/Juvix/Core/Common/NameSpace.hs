{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Juvix.Core.Common.NameSpace where

import Juvix.Library hiding (modify, toList)
import qualified Juvix.Library.HashMap as HashMap

-- TODO :: Put protected here
data T b
  = T
      { public :: HashMap.T Symbol b,
        private :: HashMap.T Symbol b
      }
  deriving (Show, Eq, Data, Functor, Foldable, Traversable)

data List b
  = List
      { publicL :: [(Symbol, b)],
        privateL :: [(Symbol, b)]
      }
  deriving (Show, Data)

-- | From represents whether the variable came from
-- the public names below us, or the private names below us
-- Also used in insertion in determining whether we should
-- place a value in the private or public map
data From b
  = Pub b
  | Priv b
  deriving (Show, Functor, Traversable, Foldable, Eq, Data)

empty :: T b
empty = T {public = HashMap.empty, private = HashMap.empty}

lookup :: Symbol -> T v -> Maybe v
lookup s T {public} =
  HashMap.lookup s public

lookupPrivate :: Symbol -> T v -> Maybe v
lookupPrivate s T {private} =
  HashMap.lookup s private

-- [lookupInternal] looksup the symbol from the private
-- namespace first, as although variables can't be in
-- both namespaces at once, there could exist an inner module
-- which is private and a public module via a file that
-- we wish to keep out of conflict (implicit shadow).
lookupInternal :: Symbol -> T v -> Maybe (From v)
lookupInternal s T {public, private} =
  fmap Priv (HashMap.lookup s private)
    <|> fmap Pub (HashMap.lookup s public)

insert :: From Symbol -> v -> T v -> T v
insert (Pub s) = insertPublic s
insert (Priv s) = insertPrivate s

insertPublic :: Symbol -> v -> T v -> T v
insertPublic sym val t =
  t {public = HashMap.insert sym val (public t)}

insertPrivate :: Symbol -> v -> T v -> T v
insertPrivate sym val t =
  t {private = HashMap.insert sym val (private t)}

removePrivate :: Symbol -> T v -> T v
removePrivate sym T {public, private} =
  T {private = HashMap.delete sym private, public}

removePublic :: Symbol -> T v -> T v
removePublic sym T {public, private} =
  T {public = HashMap.delete sym public, private}

remove :: From Symbol -> T v -> T v
remove (Pub sym) = removePublic sym
remove (Priv sym) = removePrivate sym

toList :: T v -> List v
toList T {public, private} =
  List {publicL = HashMap.toList public, privateL = HashMap.toList private}

toList1 :: T v -> [(Symbol, From v)]
toList1 ns =
  let List {publicL, privateL} = toList ns
   in (second Pub <$> publicL) <> (second Priv <$> privateL)

toList1FSymb :: T v -> [(From Symbol, v)]
toList1FSymb ns =
  let List {publicL, privateL} = toList ns
   in (first Pub <$> publicL) <> (first Priv <$> privateL)

toList1' :: T v -> [(Symbol, v)]
toList1' ns =
  let List {publicL, privateL} = toList ns in publicL <> privateL

fromList :: List v -> T v
fromList List {publicL, privateL} =
  T {public = HashMap.fromList publicL, private = HashMap.fromList privateL}

extractValue :: From a -> a
extractValue (Pub aa) = aa
extractValue (Priv a) = a
