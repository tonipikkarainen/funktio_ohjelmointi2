module Week4.Exercise2 where

import Week2.Exercise2
import Week3.Exercise2


instance (Monad m, Monad n) => Monad (Product m n) where
    return x = Pair (return x) (return x)
    Pair x y >>= f = Pair (x >>= fstPair . f) (y >>= sndPair . f)
{-
Todistus: 
Left ID:

return a >>= k == Pair (return a) (return a) >>= k 
    Pair ((return a) >>= fstPair . f ) ((return a) >>= sndPair . f)
    {- koska m ja n ovat monadeja, monadi-lait pätevät niille.-}
    eli 
    [((return a) >>= fstPair . f ) == (fstPair . f) a 
    ((return a) >>= sndPair . f ) == (sndPair . f) a]
    
    == Pair ((fstPair . f) a)  ((sndPair . f) a) == f a 
Laki pätee.

Right ID:

m == Pair x y 

Pair x y >>= return == Pair (x >>= fstPair . return) (y >>= sndPair . return)==
    {- Jos x, muokataan return:lla alkuperäiseen muotoon ja otetaan
    ensimmäinen pari, tulos on x. Sama pätee y:lle. -} 
    Pair x y == m
    
Associativity:

m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h

Tämäkin pätee, sillä määrittelyssä hyödynnetään monadeja m ja n, joille laki pätevät.
Ja jos laki pätevät monadeille m ja n, ne pätevät myös monadille Product.


-}

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


instance Monad (Cont a) where
    return = pure
    Cont x >>= f = Cont y where
        y = \h -> x $ (\k -> (runCont (f k)) h)


instance (Monad m) => Monad (Star m a) where
    return = pure
    Star x >>= f = Star y where
        y = \z -> (x z >>= \k -> (runStar (f k)) z )


instance Monad (Costar m a) where
    return = pure
    Costar x >>= f = Costar y where
        y = \z -> runCostar (f (x z)) z

instance (Monad m) => Monad (Yoneda m) where
    return = pure
    Yoneda x >>= f = Yoneda y where
        y = \z -> x id >>= \k -> runYoneda (f k) z 


instance (Monad m) => Monad (Coyoneda m) where
    return = pure
    Coyoneda x y >>= f = Coyoneda id z where
        z = y >>= lowerCoyoneda . f . x 



lowerCoyoneda :: Functor f => Coyoneda f a -> f a
lowerCoyoneda (Coyoneda f m) = fmap f m