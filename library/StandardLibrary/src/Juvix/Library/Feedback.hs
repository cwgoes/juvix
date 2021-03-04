module Juvix.Library.Feedback where

import qualified Control.Monad.Trans as Trans
import Juvix.Library

-- | Keep track of messages during the compilation or REPL.
-- The implementation is based on
-- [[https://github.com/UU-ComputerScience/uu-cco/blob/master/uu-cco/src/CCO/Feedback.hs]].
data Feedback msg a
  = Succes msg a -- Indicate succes.
  | Fail msg -- Indicate a failure.
  deriving (Show)

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

-- | Monad transformer of Feedback.
data FeedbackT msg m a = FeedbackT {runFeedbackT :: m (Feedback msg a)}

instance Monad m => Functor (FeedbackT msg m) where
  fmap f mx = FeedbackT $ do
    x <- runFeedbackT mx
    return $ fmap f x

instance (Monad m, Monoid msg) => Applicative (FeedbackT msg m) where
  pure = FeedbackT . pure . pure

  aaf <*> aax = FeedbackT $ do
    af <- runFeedbackT aaf
    ax <- runFeedbackT aax
    return $ af <*> ax

instance (Monad m, Monoid msg) => Monad (FeedbackT msg m) where
  return = pure

  mmx >>= mmf = FeedbackT $ do
    mx <- runFeedbackT mmx
    case mx of
      Succes msgs x -> runFeedbackT $ mmf x
      Fail msgs -> return $ Fail msgs

instance Monoid msg => Trans.MonadTrans (FeedbackT msg) where
  lift mx = FeedbackT $ mx >>= return . return

instance (MonadIO m, Monoid msg) => MonadIO (FeedbackT msg m) where
  liftIO = lift . liftIO
