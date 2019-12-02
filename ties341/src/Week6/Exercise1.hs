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
  fmap f (WrapTraversable xs) = WrapTraversable (fmap f xs)--WrapTraversable (traverse f xs)

instance Traversable m => Foldable (WrappedTraversable m) where
  foldMap f (WrapTraversable xs) = (foldMap f xs)

instance Traversable m => Traversable (WrappedTraversable m) where
  sequenceA   = traverse id
  traverse k (WrapTraversable xs) = WrapTraversable <$> (traverse k xs)

{-
  The base library functions pertaining to foldables and 
  traversables are related to each other in several different 
  ways.
  However, this is obscured by the type class hierarchy and the 
  clever default 
definitions it hides.

  Implement the instances that witness the relations between 
  the different functions. While doing so, try to find a way 
  to avoid using the unwrapped functions with the respective 
  wrappers.
  
  This is trickier than it might seem, so be patient!
  -}