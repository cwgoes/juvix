module Juvix.Library.Feedback
  ( Feedback (..),
    FeedbackT (..),
    feedback,
    feedbackT,
    Fail.fail,
  )
where

import qualified Control.Monad.Fail as Fail
import qualified Control.Monad.Trans as Trans
import qualified Data.String as String
import Juvix.Library

-- | Keep track of messages during the compilation or REPL.
-- The implementation is based on
-- [[https://github.com/UU-ComputerScience/uu-cco/blob/master/uu-cco/src/CCO/Feedback.hs]].
data Feedback (app :: * -> *) msg a
  = Success (app msg) a -- Indicate success.
  | Fail (app msg) -- Indicate a failure.

instance Functor (Feedback app msg) where
  fmap f (Success msgs x) = Success msgs (f x)
  fmap _ (Fail msgs) = Fail msgs

instance Monoid (app msg) => Applicative (Feedback app msg) where
  pure x = Success mempty x

  Success msgs f <*> ax = case ax of
    Success msgs' x -> Success (msgs <> msgs') (f x)
    Fail msgs' -> Fail (msgs <> msgs')
  Fail msgs <*> _ = Fail msgs

instance Monoid (app msg) => Monad (Feedback app msg) where
  return = pure
  Success msgs x >>= mf = case mf x of
    Success msgs' x' -> Success (msgs <> msgs') x'
    Fail msgs' -> Fail (msgs <> msgs')
  Fail msgs >>= _ = Fail msgs

instance
  ( Applicative app,
    Monoid (app msg),
    String.IsString msg
  ) =>
  Fail.MonadFail (Feedback app msg)
  where
  fail = Fail . pure . String.fromString

-- | Monad transformer of Feedback.
data FeedbackT app msg m a = FeedbackT {runFeedbackT :: m (Feedback app msg a)}

instance Monad m => Functor (FeedbackT app msg m) where
  fmap f mx = FeedbackT $ do
    x <- runFeedbackT mx
    return $ fmap f x

instance (Monad m, Monoid (app msg)) => Applicative (FeedbackT app msg m) where
  pure = FeedbackT . pure . pure

  aaf <*> aax = FeedbackT $ do
    af <- runFeedbackT aaf
    ax <- runFeedbackT aax
    return $ af <*> ax

instance (Monad m, Monoid (app msg)) => Monad (FeedbackT app msg m) where
  return = pure

  mmx >>= mmf = FeedbackT $ do
    mx <- runFeedbackT mmx
    case mx of
      Success msgs x -> do
        mf <- runFeedbackT $ mmf x
        case mf of
          Success msgs' x' -> return $ Success (msgs <> msgs') x'
          Fail msgs' -> return $ Fail (msgs <> msgs')
      Fail msgs -> return $ Fail msgs

instance Monoid (app msg) => Trans.MonadTrans (FeedbackT app msg) where
  lift mx = FeedbackT $ mx >>= return . return

instance (MonadIO m, Monoid (app msg)) => MonadIO (FeedbackT app msg m) where
  liftIO = lift . liftIO

instance
  ( Monad m,
    Applicative app,
    Monoid (app msg),
    String.IsString msg
  ) =>
  Fail.MonadFail (FeedbackT app msg m)
  where
  fail = FeedbackT . return . Fail.fail

-- | Provide feedback without failure.
feedback :: Applicative app => msg -> Feedback app msg ()
feedback msg = Success (pure msg) ()

-- | Provide feedback without failure for monad transformers.
feedbackT :: (Monad m, Applicative app) => msg -> FeedbackT app msg m ()
feedbackT = FeedbackT . return . feedback
