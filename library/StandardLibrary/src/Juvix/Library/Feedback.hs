module Juvix.Library.Feedback where

import Juvix.Library

-- | Keep track of messages during the compilation or REPL.
-- The implementation is based on
-- [[https://github.com/UU-ComputerScience/uu-cco/blob/master/uu-cco/src/CCO/Feedback.hs]].
data Feedback msg a
  = Succes msg a -- Indicate succes.
  | Fail msg -- Indicate a failure.

instance Functor (Feedback msg) where
  fmap f (Succes msgs x) = Succes msgs (f x)
  fmap _ (Fail msgs) = Fail msgs

instance Monoid msg => Applicative (Feedback msg) where
  pure x = Succes mempty x

  Succes msgs f <*> ax = case ax of
    Succes msgs' x -> Succes (msgs <> msgs') (f x)
    Fail msgs' -> Fail (msgs <> msgs')
  Fail msgs <*> _ = Fail msgs

instance Monoid msg => Monad (Feedback msg) where
  return = pure
  Succes msgs x >>= mf = case mf x of
    Succes msgs' x' -> Succes (msgs <> msgs') x'
    Fail msgs' -> Fail (msgs <> msgs')
  Fail msgs >>= _ = Fail msgs
