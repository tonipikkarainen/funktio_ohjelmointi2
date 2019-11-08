{-# LANGUAGE MagicHash #-}
module Exercise1 where
import Data.Bifunctor
import Data.Functor.Contravariant
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Profunctor
import Data.Void
--import GHC.Prim


newtype Endo' a = Endo' {appEndo' :: a -> a}

newtype Op' a b = Op' {getOp' :: b -> a}

newtype OmaFunktio a b = OmaFunktio {getOmaFunktio :: (->) a b}

newtype Maybe' a = Maybe' {getMaybe' :: Maybe a}
  deriving Show

newtype Either' a b = Either' {getEither' :: Either a b}
  deriving Show

newtype OmaTuple a b = OmaTuple {getTuple :: (a,b)} 
  deriving Show

newtype Lista a = Lista {getLista :: [a]}
  deriving Show


newtype OmaNonEmpty a = OmaNonEmpty {getEmpty :: NonEmpty a}
  deriving Show

newtype OmaIO a = OmaIO {getIO :: IO a}

newtype OmaMap k a = OmaMap {getMap :: Map k a}
  deriving Show
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



fmap f 

    -}

-- Todistukset puuttuu!!!

-- 1. Bool
-- Tämä ei toimi, sillä Bool tyypillä ei ole argumentteja, joten
-- kaikkien erilaisten funktoreiden funktioiden tyyppimäärittelyissä
-- esiintyvät "m a" ja "m b" jne eivät toimi. 

-- 2. Maybe a

instance Functor Maybe' where
    fmap _  (Maybe' Nothing)   =  (Maybe' Nothing)
    fmap f (Maybe' (Just a))      =  Maybe' (Just (f a))

-- Ei ole Contravariantia - 
-- Voitaisiin antaa funktiolle mayben sisältö ja saada siitä 2. argumentiksi
-- Maybe' b, mutta sen jälkeen tyyppiä a enää mitenkään. 
-- f


-- Ei ole Bi- tai profunktoria, koska datatyyppi Maybe ottaa vain yhden parametrin
-- Todistus: (Functor)
-- 1) fmap id (Maybe' Nothing) = (Maybe' Nothing) == id (Maybe' Nothing)
-- 2) fmap id (Maybe' (Just a)) =  Maybe' (Just (id a)) == Maybe' (Just (a)) ==
--    id (Maybe' (Just (a)))
-- 3) fmap (f . g) (Maybe' Nothing) = (Maybe' Nothing) == 
--      ((fmap f) . (fmap g)) (Maybe' Nothing) 
-- 4) fmap (f . g) (Maybe' (Just a)) = Maybe' (Just ((f . g) a)) ja
--    ((fmap f) . (fmap g) ) (Maybe' (Just a)) = (fmap f) (Maybe' (Just (g a))) =
--      Maybe' (Just ((f . g) a))  -- Eli samat tulee tässäkin tapauksessa.

-- 3. Either 
instance Functor (Either' a) where
  fmap f (Either' (Left x)) = (Either' (Left x))
  fmap f (Either' (Right x)) = (Either' (Right (f x)))
  

instance Bifunctor Either' where
  bimap f g (Either' (Left x)) = Either' (Left (f x))
  bimap f g (Either' (Right x)) = Either' (Right (g x))

-- 4. (,) a b
instance Functor (OmaTuple r) where
  fmap f (OmaTuple (x,y)) = OmaTuple (x, f y)

instance Bifunctor (OmaTuple) where
  bimap f g (OmaTuple (x,y)) = OmaTuple (f x, g y)

-- 5. Endo a
-- f :: a -> b
--instance Functor Endo' where
--    fmap f  (Endo' a) = Endo' (f a)

-- 6.  Funktio ja Op
-- Funktiolla (kun parametrisoidaan tuloksen yli) on funktori
-- ja Profunktori 
-- Op:lla (parametrisoidaan lähtötyypin yli) on contravariantti. 


instance Functor ((OmaFunktio) r) where
    fmap f (OmaFunktio x) = OmaFunktio (f . x) 

instance Profunctor OmaFunktio where
  dimap f g (OmaFunktio x) = OmaFunktio (g . x . f)   
  
instance Contravariant (Op' r) where
  contramap f (Op' x) = Op' (x . f)

--instance Bifunctor (Op') where
--  bimap f g x = y
--  y:n pitäisi olla tyyppiä d -> a mutta ei saada d:tä mistään.

-- 7. () - ei ole funktori, koska konstruktorilla ei ole parametreja

-- 8. [] a

instance Functor Lista where
  fmap f (Lista x) = Lista (map f x)



-- 9. NonEmpty a

instance Functor OmaNonEmpty where
  fmap f (OmaNonEmpty (x :| xs) ) = OmaNonEmpty ((f x) :| (map f xs))

-- 10. Void
-- ei ole funktori, koska konstruktorilla ei ole parametreja


-- 11. IO a

--instance Functor OmaIO where
--  fmap f (OmaIO x) = OmaIO y where 
--    y = do
--      z <- x
--      return (f z)
-- tämä primitiiveillä jos ehtii
instance Functor OmaIO where
  fmap f (OmaIO x) = OmaIO y where 
    y = (x >>= \z -> (return (f z)))


instance Functor (OmaMap a) where
  fmap f (OmaMap x)= OmaMap (Map.map f x)