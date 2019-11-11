module Week3.Exercise1 where
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Void
import Week2.Exercise1
{-
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
Instances for Map k a.-}

{-
Todistukset:

identity
pure id <*> v = v

composition
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

homomorphism
pure f <*> pure x = pure (f x)

interchange
u <*> pure y = pure ($ y) <*> u

-}

-- applicative functors??

-- Näille ei ole:
-- Bool, Endo a, (), Void, Op (koska ei ole functoria), Map
-- 2.
instance Applicative Maybe' where
    pure x = Maybe' (Just x)
    Maybe' Nothing <*> _ = Maybe' Nothing
    Maybe' (Just f) <*> Maybe' x = Maybe' (fmap f x)

-- 3. Either

instance Applicative (Either' a) where
    pure x = Either' (Right x)
    Either' (Right f) <*> Either' x = Either' (fmap f x)
    Either' (Left a) <*> x = Either' (Left a)

-- 4. (,)
instance Monoid r => Applicative (OmaTuple r) where
    pure x = OmaTuple (mempty, x)
    OmaTuple (x,f) <*> OmaTuple (y,z) = OmaTuple ( x<>y, f z)


-- 6. (-> a b ) tälle voi olla applicative functor

instance Applicative (OmaFunktio r) where
    pure x = OmaFunktio (const x)
    (OmaFunktio f) <*> (OmaFunktio g)  = 
        OmaFunktio (\x -> f x (g x)) 

instance Applicative Lista where
    pure x = Lista [x]
    Lista fs <*> Lista xs = Lista [f x | f <- fs, x <- xs ]

instance Applicative OmaNonEmpty where
    pure x = OmaNonEmpty  (x :| [])
    OmaNonEmpty (g:|fs) <*> OmaNonEmpty (y:|xs) 
        = OmaNonEmpty w where
            h = [f x | f <- g:fs, x <- y:xs]
            w = case h of (x:xs) -> x:|xs

instance Applicative OmaIO where
    pure x = OmaIO (return x)
    OmaIO f <*> OmaIO x = OmaIO y where
        y = do
            f' <- f
            x' <- x
            return (f' x')
