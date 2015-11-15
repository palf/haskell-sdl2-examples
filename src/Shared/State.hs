module Shared.State (
    into,
    kick,
    (~>>),
    (~>~)
) where

import Control.Monad.State

into :: (Monad m, MonadTrans t, MonadState b (t m)) => m a -> (a -> b -> b) -> t m b
into source f = lift source >>= modify . f >> get

kick :: (Monad (t m), Monad m, MonadTrans t) => t m a -> (a -> m b) -> t m b
kick m f = m >>= lift . f

push :: (Monad (t m), Monad m, MonadTrans t) => t m a -> (a -> m b) -> t m a
push m f = m >>= \x -> (lift . f) x >> return x

infixl 4 ~>>
(~>>) :: (Monad (t m), Monad m, MonadTrans t) => t m a -> (a -> m b) -> t m b
(~>>) = kick

infixl 4 ~>~
(~>~) :: (Monad (t m), Monad m, MonadTrans t) => t m a -> (a -> m b) -> t m a
(~>~) = push

