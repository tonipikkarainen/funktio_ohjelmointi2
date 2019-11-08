{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Exercise2 where
import Data.Bifunctor
import Data.Functor.Contravariant
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Profunctor hiding (Star, Costar)
import Data.Void
import Exercise1

-- Todistukset puuttuu




data Sum m n a = InL (m a) | InR (n a)
  deriving Show

instance (Functor m, Functor n) => Functor (Sum m n) where
   fmap f (InL x) = InL (fmap f x)
   fmap f (InR x) = InR (fmap f x)

 -- m on funktori, joten fmap f (m a) = m b   


data Product m n a = Pair {fstPair :: m a, sndPair :: n a}
  deriving Show

instance (Functor m, Functor n) => Functor (Product m n) where
  fmap f (Pair {fstPair = x, sndPair = y }) 
    = Pair { fstPair = (fmap f x) , sndPair = (fmap f y) }


newtype Identity a = Identity {runIdentity :: a}
  deriving Show

instance Functor Identity where
  fmap f (Identity {runIdentity = x }) = Identity {runIdentity = f x}

newtype Compose m n a = Compose {getCompose :: m (n a)}
  deriving Show

instance (Functor m, Functor n) => Functor (Compose m n) where
  fmap f (Compose { getCompose = x }) = Compose { getCompose = fmap (fmap f) x }

-- \(fmap f x) -> fmap f 
-- m funktori joten 
-- fmap f (m a) = m b ja b funktori joten fmap 
-- fmap f (fmap f x) 
newtype Const a b = Const {getConst :: a}
  deriving Show

instance Functor (Const a) where
  fmap f (Const {getConst = x}) = (Const {getConst = x})

data Proxy a = Proxy
  deriving Show

instance Functor Proxy where
  fmap f Proxy = Proxy

newtype State a b = State {runState :: a -> (b, a)}

instance Functor (State a) where 
  fmap f (State {runState = x}) = State {runState = (g . x)} where
    g (x, y) = ((f x), y)
 
newtype Cont a b = Cont {runCont :: (b -> a) -> a}

-- löysin netistä mutta en ymmärrä:
instance Functor (Cont a) where
  fmap f (Cont {runCont = x}) = Cont {runCont = x . (. f)}

-- ?? 

newtype Star m a b = Star {runStar :: a -> m b}

instance (Functor m) => Functor (Star m r)  where
  fmap f (Star x) = Star ((fmap f) . x )


newtype Costar m a b = Costar {runCostar :: m a -> b}

instance (Functor m) => Functor (Costar m a) where
  fmap f (Costar x) = Costar (f . x)

newtype Yoneda m a = Yoneda {runYoneda :: forall b. (a -> b) -> m b}

instance (Functor m) => Functor (Yoneda m) where
  fmap f (Yoneda {runYoneda = x}) = Yoneda {runYoneda = x . (. f)}

data Coyoneda m a = forall b. Coyoneda (b -> a) (m b)

instance (Functor m) => Functor (Coyoneda m) where
  fmap f (Coyoneda x y) = Coyoneda (f . x) y
