module Juvix.Library.Sexp
  ( module Juvix.Library.Sexp.Types,
    module Juvix.Library.Sexp.Parser,
    foldPred,
    foldr,
    foldr1,
    butLast,
    last,
    list,
    listStar,
    addMetaToCar,
    car,
    cdr,
    atom,
    number,
    isAtomNamed,
    nameFromT,
    atomFromT,
    groupBy2,
    assoc,
  )
where

import Juvix.Library hiding (foldr, list, show, toList)
import qualified Juvix.Library as Std
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Sexp.Parser
import Juvix.Library.Sexp.Types
import Prelude (error)

foldPred :: T -> (NameSymbol.T -> Bool) -> (Atom -> T -> T) -> T
foldPred t pred f =
  case t of
    Cons (Atom atom@(A name _)) xs
      | pred name ->
        foldPred (f atom xs) pred f
    Cons cs xs ->
      Cons (foldPred cs pred f) (foldPred xs pred f)
    Nil -> Nil
    Atom a -> Atom a

foldr :: (T -> p -> p) -> p -> T -> p
foldr f acc ts =
  case ts of
    Cons a as -> f a (foldr f acc as)
    Atom ____ -> f ts acc
    Nil -> acc

foldr1 :: (T -> T -> T) -> T -> Maybe T
foldr1 f (Cons x xs) = Just $ unsafe (Cons x xs)
  where
    unsafe ts =
      case ts of
        Cons a Nil -> a
        Cons a cds -> f a (unsafe cds)
        _ -> error "doesn't happen"
foldr1 _ _empty = Nothing

butLast :: T -> T
butLast (Cons _ Nil) = Nil
butLast (Cons x xs) = Cons x (butLast xs)
butLast (Atom a) = Atom a
butLast Nil = Nil

last :: T -> T
last (Cons x Nil) = x
last (Cons _ xs) = last xs
last (Atom a) = Atom a
last Nil = Nil

list :: Foldable t => t T -> T
list = Std.foldr Cons Nil

listStar :: [T] -> T
listStar = fromMaybe Nil . foldr1May Cons

addMetaToCar :: Atom -> T -> T
addMetaToCar (A _ lineInfo) (Cons (Atom (A term _)) xs) =
  Cons (Atom (A term lineInfo)) xs
addMetaToCar _ xs = xs

car :: T -> T
car (Cons x _) = x
car Nil = Nil
car (Atom a) = Atom a

cdr :: T -> T
cdr (Cons _ xs) = xs
cdr Nil = Nil
cdr (Atom a) = Atom a

cadr :: T -> T
cadr = car . cdr

atom :: NameSymbol.T -> T
atom x = Atom $ A x Nothing

number :: Integer -> T
number n = Atom $ N n Nothing

isAtomNamed :: T -> NameSymbol.T -> Bool
isAtomNamed (Atom (A name _)) name2 = name == name2
isAtomNamed _ _ = False

atomFromT :: T -> Maybe Atom
atomFromT (Atom a) = Just a
atomFromT _ = Nothing

nameFromT :: T -> Maybe NameSymbol.T
nameFromT (Atom (A name _)) = Just name
nameFromT _ = Nothing

assoc :: T -> T -> Maybe T
assoc t (car :> cdr)
  | t == car = Just (cadr car)
  | otherwise = assoc t cdr
assoc _ Nil = Nothing
assoc _ Atom {} = Nothing

groupBy2 :: T -> T
groupBy2 (a1 :> a2 :> rest) =
  list [a1, a2] :> groupBy2 rest
groupBy2 _ = Nil
