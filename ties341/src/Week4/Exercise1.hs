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


-}

-- Näille ei ole:
-- Bool, Endo a, (), Void, Op (koska ei ole functoria), Map

-- Maybe:
instance Monad Maybe' where
    return x = Maybe' (Just x) 
    Maybe' (Just x) >>= f = f x
    Maybe' Nothing >>= f = Maybe' Nothing

-- Either
instance Monad (Either' a) where
    return x = Either' (Right x)
    Either' (Right x) >>= f =  f x
    Either' (Left x) >>= f = Either' (Left x)
-- (,)
instance (Monoid r) => Monad (OmaTuple r) where
    return = pure
    OmaTuple (x, y) >>= f = case f y of OmaTuple (z, b) -> OmaTuple (x<>z , b)
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
            


