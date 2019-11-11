{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Exercise1 where
import Data.Bifunctor
import Data.Functor.Contravariant
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Profunctor
import Data.Void
import GHC.IO
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




-- 1. Bool
-- Tämä ei toimi, sillä Bool tyypillä ei ole parametreja, joten
-- kaikkien erilaisten funktoreiden funktioiden tyyppimäärittelyissä
-- esiintyvät "m a" ja "m b" jne eivät toimi. 

-- 2. Maybe a

instance Functor Maybe' where
    fmap _  (Maybe' Nothing)   =  (Maybe' Nothing)
    fmap f (Maybe' (Just a))      =  Maybe' (Just (f a))




-- Todistus: (Functor)
-- 1) fmap id (Maybe' Nothing) = (Maybe' Nothing) == id (Maybe' Nothing)
-- 2) fmap id (Maybe' (Just a)) =  Maybe' (Just (id a)) == Maybe' (Just (a)) ==
--    id (Maybe' (Just (a)))
-- 3) fmap (f . g) (Maybe' Nothing) = (Maybe' Nothing) 
--    ((fmap f) . (fmap g)) (Maybe' Nothing) =
--      fmap f (fmap g (Maybe' Nothing)) = fmap f (Maybe' Nothing) =
--          (Maybe' Nothing)            
-- 4) fmap (f . g) (Maybe' (Just a)) = Maybe' (Just ((f . g) a)) ja
--
--    ((fmap f) . (fmap g) ) (Maybe' (Just a)) = fmap f (Maybe' (Just (g a))) =
--      Maybe' (Just (f (g a)) =
--           Maybe' (Just ((f . g) a))  -- Eli samat tulee tässäkin tapauksessa.

-- 3. Either 
instance Functor (Either' a) where
  fmap f (Either' (Left x)) = (Either' (Left x))
  fmap f (Either' (Right x)) = (Either' (Right (f x)))
{-
 Todistus:
 ID: 

 1) fmap id (Either' (Left x)) = (Either' (Left x)) == id  (Either' (Left x))
 2) fmap id (Either' (Right x)) = (Either' (Right (id x))) 
 == (Either' (Right (x))) == id (Either' (Right (x)))

 f . g :
3) fmap (f.g) (Either' (Left x)) = (Either' (Left x)) ja
  (fmap f) . (fmap g) (Either' (Left x)) ==
    fmap f (Either' (Left x)) == (Either' (Left x)) 
4) fmap (f.g) (Either' (Right x)) = (Either' (Right (f . g) x)) ja
  (fmap f) . (fmap g) (Either' (Right x)) =
    fmap f (Either' (Right g x)) = Either' (Right f ( g x)) =
      Either' (Right (f . g) x))
  
 -}

instance Bifunctor Either' where
  bimap f g (Either' (Left x)) = Either' (Left (f x))
  bimap f g (Either' (Right x)) = Either' (Right (g x))

{-
Todistus:
ID:
1) bimap id id (Either' (Left x)) == id (Either' (Left x)) selvästi
2) bimap id id (Either' (Right x)) == id (Either' (Right x)) selvästi

f . g :

3) bimap (f . g) ( h . i )  (Either' (Left x)) ==
    (bimap f h ) . (bimap g i) (Either' (Left x))
  on Eitherin funktorin perusteella selvästi totta.
4) bimap (f . g) ( h . i )  (Either' (Right x)) ==
    (bimap f h ) . (bimap g i) (Either' (Right x))

-}

-- 4. (,) a b
instance Functor (OmaTuple r) where
  fmap f (OmaTuple (x,y)) = OmaTuple (x, f y)
{-
Todistus:
ID:
fmap id (OmaTuple (x,y)) = (OmaTuple (x, id y)) = id (OmaTuple (x,y)) 

f . g
fmap (f . g) (OmaTuple (x,y)) = (OmaTuple (x, (f.g) y)) ja
( fmap f ) . ( fmap g) (OmaTuple (x,y)) = fmap f (OmaTuple (x, g y)) ==
  (OmaTuple (x, (f.g) y))

-}

instance Bifunctor (OmaTuple) where
  bimap f g (OmaTuple (x,y)) = OmaTuple (f x, g y)
{-
Todistus : 
ID - selvästi pätee.

f . g  = tämäkin pätee selvästi, kun päti funktorillekin.
-}

-- 5. Endo a
-- Ei ole funktoria

-- 6.  Funktio ja Op
-- Funktiolla (kun parametrisoidaan tuloksen yli) on funktori
-- ja Profunktori 
-- Op:lla (parametrisoidaan lähtötyypin yli) on contravariantti. 


instance Functor ((OmaFunktio) r) where
    fmap f (OmaFunktio x) = OmaFunktio (f . x) 
{-
Todistus:
ID:
koska id . x = x , niin pätee.

f.g :
fmap (f.g) (OmaFunktio x) = OmaFunktio ( (f.g) . x) = OmaFunktio ( f. (g . x)) 
(fmap f).(fmap g) (OmaFunktio x) =fmap f OmaFunktio ( g . x) =
   OmaFunktio ( f. (g . x)) 
-}

instance Profunctor OmaFunktio where
  dimap f g (OmaFunktio x) = OmaFunktio (g . x . f)
  
{-
Todistus:

ID:
dimap id id (OmaFunktio x) = OmaFunktio (id . id . f) - pätee koska id . f = f ja 
f . id = f  

f.g:
dimap (f.g) (h.i) (OmaFunktio x)  = OmaFunktio ( (h.i) . x . (f.g ) ) =
  OmaFunktio ( h . i . x . f . g ) ) 

(dimap g h) . (dimap f i) (OmaFunktio x)  = dimap g h (OmaFunktio (i . x . f ) ) =
  (OmaFunktio (h . i . x . f . g ) 

-}
  
instance Contravariant (Op' r) where
  contramap f (Op' x) = Op' (x . f)

{-
Todistukset:

ID: 
contramap id (Op' x) = id (Op' x) (selvästi - perustelu jo aiemmissa kohdissa)

f.g: 
contramap (f.g) (Op' x) = (Op'  ( x. (f.g) ) = (Op' (x . f . g)
(contramap g) . (contramap f) (Op' x) = contramap g (Op' (x . f)) =
  (Op' (x . f . g)

-}  


-- 7. () - ei ole funktori, koska konstruktorilla ei ole parametreja

-- 8. [] a

instance Functor Lista where
  fmap f (Lista x) = Lista (map f x)
{-
Todistus:

ID - pätee selvästi, kun mapataan listaa id:llä.

f.g:
fmap (f.g) (Lista x) = Lista (map (f.g) x)

(fmap f).(fmap g) (Lista x) = fmap f Lista (map g x) =
  Lista (map f (map g x)) = Lista (map (f.g) x)
   Viimeinen kohta sanallisesti: Jos ensin map:ätään lista g:llä
   ja sitten f:llä, se on sama asia kuin jos map:ätään
   yhdistetyllä funktiolla (f . g) - tässä vaan suoritetaan funktio
   g ja f tietylle alkiolle peräkkäin.
-}


-- 9. NonEmpty a

instance Functor OmaNonEmpty where
  fmap f (OmaNonEmpty (x :| xs) ) = OmaNonEmpty ((f x) :| (map f xs))
{-
Todistukset: 

pätevät tällä, koska päti listoille.
-}

-- 10. Void
-- ei ole funktori, koska konstruktorilla ei ole parametreja


-- 11. IO a


--instance Functor OmaIO where
--  fmap f (OmaIO x) = OmaIO y where 
--    y = (x >>= \z -> (return (f z)))

instance Functor OmaIO where
    fmap f (OmaIO x) = OmaIO y where
      y = IO $ \s -> case unIO x s of (# s1 , a #) -> unIO ( (return . f) a ) s1   

{-
Todistukset:
ID:
Selvästi pätee, sillä return . id  == return

f.g : 

fmap (f.g) (OmaIO x) = OmaIO y where
      y = IO $ \s -> case unIO x s of (# s1 , a #) -> unIO ( (return . (f.g)) a ) s1

(fmap f) . (fmap  g) (OmaIO x) = fmap f (OmaIO y) where     
      y = IO $ \s -> case unIO x s of (# s1 , a #) -> unIO ( (return (g a) ) s1 

= OmaIO z where     
      z = IO $ \s -> case unIO x s of (# s1 , a #) -> unIO ( (return (f (g a)) ) s1 
= OmaIO z where     
      z = IO $ \s -> case unIO x s of (# s1 , a #) -> unIO ( (return . (f .g)) a)  s1 
      -}
instance Functor (OmaMap a) where
  fmap f (OmaMap x)= OmaMap (Map.map f x)
{-
Pätevät tälle, koska pätivät listoille. Tässä vaan
map:ätään listamaisesti arvo-alkioiden yli. 
-}
