{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Week4.Exercise1 where


import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import GHC.IO (IO (..))
import Week2.Exercise1
import Week3.Exercise1

{-
class Applicative m => Monad m where
Monad methods:

(>>=) :: forall a b. m a -> (a -> m b) -> m b 
(>>) :: forall a b. m a -> m b -> m b 
return :: a -> m a
fail :: String -> m a



Instances for Maybe a.
Instances for Either a b.
Instances for (,) a b.

Instances for (->) a b 

Instances for [] a.
Instances for NonEmpty a.

Instances for IO a.

Monad laws:
Left ID: return a >>= k  =  k a
Right ID: m >>= return  =  m
Associativity: m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h


-}

-- Näille ei ole:
-- Bool, Endo a, (), Void, Op (koska ei ole functoria), Map

-- Maybe:
instance Monad Maybe' where
    return x = Maybe' (Just x) 
    Maybe' (Just x) >>= f = f x
    Maybe' Nothing >>= f = Maybe' Nothing
{-
Todistus:
Left ID:
return a on aina -> Maybe' (Just a) ja 
Maybe' (Just a) >>= k == k a,
joten pätee.

Right ID: 
Jos m == Maybe' Nothing,
m >>= f == m (aina)
ja
Jos m == Maybe' (Just x)
m >>= return == return x == Maybe' (Just x) == m,
joten laki pätee.

Associativity:
1) m == Maybe' (Just z)
(m >>= k ) >>= h == (k z) >>= h 
    1.1) k z = Maybe' (Just y) :
    (k x) >>= h == h y
    1.2) k z = Maybe' Nothing
    (k x) >>= h == Maybe' Nothing

m >>= (\x -> k x >>= h) == (\x -> k x >>= h) z ==
    k z >>= h ... ja tästä jatkuu samoin kuin yläpuolella.

2) Jos m == Maybe' Nothing --> nähdään melko helposti, että
molemmista vaihtoehdoista tulee Maybe' Nothing tulokseksi.

Lait siis pätevät.

-}
-- Either
instance Monad (Either' a) where
    return x = Either' (Right x)
    Either' (Right x) >>= f =  f x
    Either' (Left x) >>= f = Either' (Left x)
{-
Todistus:
Left ID:
return a on aina -> Either' (Right a) ja 
Either' (Right a) >>= k == k a,
joten pätee.

Right ID: 
Jos m == Either' (Left x),
m >>= f == m (aina)
ja
Jos m == Either' (Right x), niin laki pätee
samankaltaisella päättelyllä kuin Maybe':lle.


Associativity:
Tämäkin menee lähes identtisesti Mayben' kanssa.

Lait siis pätevät.


-}
-- (,)
instance (Monoid r) => Monad (OmaTuple r) where
    return = pure
    OmaTuple (x, y) >>= f = case f y of OmaTuple (z, b) -> OmaTuple (x<>z , b)
{-
Todistus:
Left ID:
return a on aina ->  OmaTuple (mempty, a) ja 
koska mempty <> x == x, niin

OmaTuple (mempty, a)  >>= k == k a,
joten pätee.

Right ID: 
Jos m == OmaTuple (x, y),
m >>= return == OmaTuple (mempty <> x, y) == m

Laki pätee.



Associativity:
m ==  OmaTuple (x, y)
* (k y = OmaTuple (z,b))
* (h b = OmaTuple (s,c))
1) (m >>= k) >>= h == OmaTuple (x <> z, b) >>= h ==
    OmaTuple (x <> z <> s , c) 
2) m >>= (\x -> k x >>= h) == (\x -> k x >>= h) y ==
    k y >>= h ==  OmaTuple (x <> z <> s , c) 


Lait siis pätevät.

-}

-- Muut todistukset samalla idealla.
-- (-> )

instance Monad (OmaFunktio r) where
    return = pure 
    OmaFunktio x >>= f  = OmaFunktio (\z -> (getOmaFunktio (f (x z)) z))

--f >>= k = \ r -> k (f r) r
-- x :: m a = r -> a
-- f :: a -> m b :: a -> (r -> b)
-- _ :: a
-- z :: r
-- x z :: a
-- f (x z) :: r -> b
-- (\z -> f (x z)) :: r -> Omafunktio ...

instance Monad Lista where
    return = pure
    Lista x >>= f =  Lista (concat (map getLista (map f x))) 


instance Monad OmaNonEmpty where
    return = pure
    OmaNonEmpty x >>= f = y where
        z = NonEmpty.toList x
        listaWithNon = map getEmpty (map f z)
        listaWithLista = map NonEmpty.toList listaWithNon
        y = OmaNonEmpty (NonEmpty.fromList (concat listaWithLista))


instance Monad OmaIO where
    return x=  OmaIO (IO (\s -> (# s, x #)))
    OmaIO x >>= f = OmaIO y where 
        unIO (IO k) = k
        y = IO $ \s -> case unIO x s of (# s1 , a #) -> unIO (getIO (f a)) s1 
            


