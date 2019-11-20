module Week4.Exercise2 where

import Week2.Exercise2
import Week3.Exercise2
-- Coyoneda puuttuu!
{-
Instances for Sum m n a, given instances for m and n.  (-)
Instances for Product m n a, given instances for m and n. (+)
Instances for Identity a. (+)
Instances for Compose m n a, given instances for m and n. (-) ei ilmeisesti ole (-)
Instances for Const a b. (-) tämäkään ei monadi?
Instances for Proxy a.  (+)
Instances for State a b. (+)
Instances for Cont a b. (?)
Instances for Star m a b, given instances for m. (+)
Instances for Costar m a b. (+)
Instances for Yoneda m a, given instances for m. (+)
Instances for Coyoneda m a, given instances for m.
-}

instance (Monad m, Monad n) => Monad (Product m n) where
    return = pure
    Pair x y >>= f = Pair (x >>= fstPair . f) (y >>= sndPair . f)

instance Monad Identity where
    return = pure
    Identity x >>= f = f x

instance Monad Proxy where
    return = pure
    Proxy >>= f = Proxy

instance Monad (State a) where
    return = pure
    State x >>= f = State y where
        y = \z -> case x z of (t,s) -> runState (f t) s  

-- newtype Cont a b = Cont {runCont :: (b -> a) -> a}
-- mistä saadaan b - f:ään?
--instance Monad (Cont a) where
--    return = pure
--    Cont x >>= f = Cont y where
--        y = \h -> runCont (f b) $

-- star m a b :: a -> m b

instance (Monad m) => Monad (Star m a) where
    return = pure
    Star x >>= f = Star y where
        y = \z -> (x z >>= \k -> (runStar (f k)) z )
--newtype Costar m a b = Costar {runCostar :: m a -> b}
instance Monad (Costar m a) where
    return = pure
    Costar x >>= f = Costar y where
        y = \z -> runCostar (f (x z)) z

-- newtype Yoneda m a = Yoneda {runYoneda :: forall b. (a -> b) -> m b}
instance (Monad m) => Monad (Yoneda m) where
    return = pure
    Yoneda x >>= f = Yoneda y where
        y = \z -> x id >>= \k -> runYoneda (f k) z 

-- data Coyoneda m a = forall b. Coyoneda (b -> a) (m b)
{-instance (Monad m) => Monad (Coyoneda m) where
    return = pure
    Coyoneda x y >>= f = Coyoneda id z where
        unCoy (Coyoneda f g) = g
        z = y >>=  (f . x) -}