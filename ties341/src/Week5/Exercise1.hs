module Week5.Exercise1 where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map (..))
import qualified Data.Map as Map
--import Week1.Exercise1
import Week2.Exercise1
import Week3.Exercise1


{-
sequenceA :: Applicative f => t (f a) -> f (t a)
foldMap :: Monoid m => (a -> m) -> t a -> m

Instances for Bool.
Instances for Maybe a.
Instances for Either a b.
Instances for (,) a b.
Instances for Endo a.
Instances for (->) a b and Op a b.
Instances for ().
Instances for [] a.
Instances for NonEmpty a.
Instances for Void.
Instances for IO a.
Instances for Map k a.
-}

instance Foldable Maybe' where
    foldMap f (Maybe' (Just x)) = f x
    foldMap f (Maybe' Nothing) = mempty

instance Traversable Maybe' where
    sequenceA (Maybe' (Just x)) = fmap (\y -> Maybe' (Just y)) x
    sequenceA (Maybe' (Nothing)) = pure (Maybe' Nothing)
--

instance Foldable (Either' a) where 
    foldMap f (Either' (Right x) ) = f x
    foldMap f ((Either' (Left x) )) = mempty

instance Traversable (Either' a) where
    sequenceA (Either' (Right x)) = 
        fmap (\y -> Either' (Right y)) x
    sequenceA (Either' (Left x)) = pure (Either' (Left x))

--


