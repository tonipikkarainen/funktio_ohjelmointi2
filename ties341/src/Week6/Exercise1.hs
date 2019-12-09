module Week6.Exercise1 where

import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity

--sequenceA :: Applicative f => t (f a) -> f (t a)
--foldMap :: Monoid m => (a -> m) -> t a -> m
--traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
newtype WrappedTraversable m a = WrapTraversable {unwrapTraversable :: m a}
-- fmap :: Functor f => (a -> b) -> f a -> f b

instance Traversable m => Functor (WrappedTraversable m) where
  fmap f = runIdentity . traverse (Identity . f)
  
instance Traversable m => Foldable (WrappedTraversable m) where
  foldMap f = getConst . traverse (Const . f)
  
instance Traversable m => Traversable (WrappedTraversable m) where
  sequenceA   = traverse id
  traverse k (WrapTraversable xs) = WrapTraversable <$> (traverse k xs)

