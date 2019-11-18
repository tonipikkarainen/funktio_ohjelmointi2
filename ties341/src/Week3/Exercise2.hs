{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Week3.Exercise2 where

import Week2.Exercise2



-- Puuttuu: Sum, Cont

-- Sum - en keksi
-- Mikä otetaan lopulliseksi konstruktoriksi?
{-
instance (Applicative m, Applicative n) => Applicative (Sum m n) where
    pure x = InL (pure x)
    InL f <*> InL x = InL (f <*> x)
    InR f <*> InL x = InR (f <*> x) ...
-}

-- Product
instance (Applicative m, Applicative n) => Applicative (Product m n) where
    pure x = Pair (pure x) (pure x)
    Pair f g <*> Pair x y = Pair (f <*> x) (g <*> y)

-- Identity

instance Applicative Identity where
    pure x = Identity x
    Identity f <*> Identity x = Identity (f x)

-- Compose

instance (Applicative m, Applicative n) => Applicative (Compose m n) where
    pure x = Compose (pure (pure x))
    Compose f <*> Compose x = Compose ((fmap (<*>) f) <*> x) 

-- <*> :: h (a -> b) -> h a -> h b
-- f :: m (n (a -> b))
-- <*> menee f:n sisälle, joten :
-- (fmap (<*>) f) :: m ((n a) ->  (n b))
--  ja 
-- (fmap (<*>) f) <*> x :: m (n b)

-- Const

-- Jos a monoidi niin toimii
instance (Monoid a) => Applicative (Const a) where
    pure x = Const mempty
    Const x <*> Const y = Const (x <> y)

-- Proxy    
instance Applicative (Proxy) where
    pure x = Proxy
    Proxy <*> Proxy = Proxy

-- State    
instance Applicative (State r) where
    pure x = State (\z -> (x, z)) 
    State f <*> State x = State (\z -> ((fst (f z) (fst (x z)), z )))

-- Cont 
--newtype Cont a b = Cont {runCont :: (b -> a) -> a}    
instance Applicative (Cont r) where 
   pure x = Cont (\f -> f x)
   Cont f <*> Cont x = Cont $ \h -> f $ \k -> x $ \z -> h (k z)


-- Star
-- newtype Star m a b = Star {runStar :: a -> m b}
instance (Applicative m) => Applicative (Star m r) where
    pure x = Star (\z -> (pure x))
    Star f <*> Star x = Star (\z -> ((f z) <*> (x z)))


-- Costar
-- newtype Costar m a b = Costar {runCostar :: m a -> b}
instance (Applicative m) => Applicative (Costar m r) where
    pure x = Costar (\_ -> x)
    Costar f <*> Costar x = Costar (\z -> (f z) (x z))

-- Yoneda 
--newtype Yoneda m a = Yoneda {runYoneda :: forall b. (a -> b) -> m b}
instance (Applicative m) => Applicative (Yoneda m) where
    pure x = Yoneda (\f -> pure (f x)) 
    Yoneda f <*> Yoneda x = Yoneda ( \h-> f (h .) <*> x id)



-- Coyoneda 
instance (Applicative m) => Applicative (Coyoneda m) where
    pure x = Coyoneda (const x) (pure x)
    Coyoneda f x <*> Coyoneda y h = Coyoneda id (g <$> x <*> h) where
        g = \z j-> f z (y j) 

