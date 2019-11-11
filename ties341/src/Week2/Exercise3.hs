module Week2.Exercise3 where
-- Tekijä: Toni Pikkarainen
-- 8.11.2019
{-
 class Functor m where
  fmap :: (a -> b) -> m a -> m b

class Contravariant m where
    contramap :: (a -> b) -> m b -> m a
  
class Bifunctor m where
  bimap :: (a -> b) -> (c -> d) -> m a c -> m b d

class Profunctor m where
  dimap :: (a -> b) -> (c -> d) -> m b c -> m a d

funktorilait: n (g . f ) = n(g) . n(f)
n (id ) = id

-}
import Control.Monad (join)
import Data.Bifunctor
import Data.Functor.Contravariant
import Data.Profunctor


newtype WrappedBifunctor m a b = WrapBifunctor {unwrapBifunctor :: m a b}
  deriving Show

newtype WrappedProfunctor m a b = WrapProfunctor {unwrapProfunctor :: m a b}
  deriving Show

newtype Flip m a b = Flip {runFlip :: m b a}
  deriving Show

newtype Join m a = Join {runJoin :: m a a}

instance Bifunctor m => Functor (WrappedBifunctor m r) where
  fmap f (WrapBifunctor x) = WrapBifunctor ( bimap id f x )  

instance Profunctor m => Functor (WrappedProfunctor m a) where
  fmap f (WrapProfunctor x) = WrapProfunctor ( dimap id f x )  

instance Bifunctor m => Functor (Flip m a) where
  fmap f (Flip x) = (Flip (bimap f id x) ) 

instance Profunctor m => Contravariant (Flip m r) where
  contramap f (Flip x) = Flip (dimap f id x)

instance Bifunctor m => Bifunctor (Flip m) where
  bimap f g (Flip x) = Flip (bimap g f x)
-- f: a -> b
-- g: c -> d
-- x = m c a
-- y = m d b
-- bimap: (c -> d) (a -> b) m c a -> m d b => Flip m b d


instance Bifunctor m => Functor (Join m) where
  fmap f (Join x) = Join (bimap f f x)