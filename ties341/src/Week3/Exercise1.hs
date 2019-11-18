module Week3.Exercise1 where
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Void
import Week2.Exercise1

-- Tein todistukset vain Maybe':lle, koska käytin tällä
-- viikolla aikaani mielenkiintoiseen bonus-tehtävään.

{-

identity
pure id <*> v = v

composition
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

homomorphism
pure f <*> pure x = pure (f x)

interchange
u <*> pure y = pure ($ y) <*> u

-}



-- Näille ei ole:
-- Bool, Endo a, (), Void, Op (koska ei ole functoria), Map
-- 2.
instance Applicative Maybe' where
    pure x = Maybe' (Just x)
    Maybe' Nothing <*> _ = Maybe' Nothing
    Maybe' (Just f) <*> Maybe' x = Maybe' (fmap f x)

{-
Todistukset:
Identity:

1) v == Maybe' (Just x)

pure id <*> Maybe' (Just x) == Maybe' (Just id) <*> Maybe' (Just x)
              == Maybe' (fmap id (Just x)) == Maybe' (Just x)== v
2) v == Maybe' Nothing
--> tästä tulee selvästi myös sama tulos.

Composition:
1) 
u == Maybe' (Just f)
v == Maybe' (Just g)
w == Maybe' (Just x)

pure (.) <*> u <*> v <*> w ==
    Maybe' (Just (.)) <*> Maybe' (Just f) <*> Maybe' (Just g) <*> Maybe' (Just x) ==
        Maybe' (fmap (.) Just f) <*> Maybe' (Just g) <*> Maybe' (Just x) ==
            Maybe' (Just (f . )) <*> Maybe' (Just g) <*> Maybe' (Just x) ==
                Maybe' (fmap (f .) (Just g)) <*> Maybe' (Just x) ==
                    Maybe' (Just (f.g)) <*> Maybe' (Just x) ==
                        Maybe' (fmap (f.g) (Just x))

u <*> (v <*> w) == Maybe' (Just f) <*> (Maybe' (Just g) <*> Maybe' (Just x)) ==
    Maybe' (fmap f (Just g)) <*> Maybe' (Just x) == 
        Maybe' (Just (f.g)) <*> Maybe' (Just x) ==
            Maybe' (fmap (f.g) (Just x))

            --> Laki pätee
2) 
Jos joku - u, v tai w - on Maybe' Nothing, molemmat lausekkeet saavat arvon
Maybe' Nothing, koska ->
Maybe' Nothing <*> _ = Maybe' Nothing ja
fmap _  (Maybe' Nothing)   =  (Maybe' Nothing)



Homomorphism:

pure f <*> pure x == Maybe' (Just f) <*> Maybe' (Just x) ==
    Maybe' (fmap f (Just x)) == Maybe' (Just (f x))

pure (f x) == Maybe' Just (f x) 

--> laki pätee.

Interchange:
1) 
u == Maybe' (Just f)

Maybe' (Just f) <*> pure y == 
    Maybe' (Just (f y))
    
    
pure ($ y) <*> u == Maybe' (Just f) == 
    Maybe' (Just ($ y)) <*> Maybe' (Just f) ==
        Maybe' (fmap ($ y) (Just f)) ==
            Maybe' (Just (($ y) f) ) ==
                Maybe' (Just (f $ y)) ==
                   Maybe' (Just (f y)) 
                   --> laki pätee

2) 
u == Maybe' Nothing

Molemmissa tapauksissa vastaus on Maybe' Nothing samoilla perusteilla kuin
Composition-kohdassa.
-}
-- 3. Either

instance Applicative (Either' a) where
    pure x = Either' (Right x)
    Either' (Right f) <*> Either' x = Either' (fmap f x)
    Either' (Left a) <*> x = Either' (Left a)

-- 4. (,)
instance Monoid r => Applicative (OmaTuple r) where
    pure x = OmaTuple (mempty, x)
    OmaTuple (x,f) <*> OmaTuple (y,z) = OmaTuple ( x<>y, f z)


-- 6. (-> a b ) 

instance Applicative (OmaFunktio r) where
    pure x = OmaFunktio (const x)
    (OmaFunktio f) <*> (OmaFunktio g)  = 
        OmaFunktio (\x -> f x (g x)) 
-- Lista
instance Applicative Lista where
    pure x = Lista [x]
    Lista fs <*> Lista xs = Lista [f x | f <- fs, x <- xs ]
-- Nonempty
instance Applicative OmaNonEmpty where
    pure x = OmaNonEmpty  (x :| [])
    OmaNonEmpty (g:|fs) <*> OmaNonEmpty (y:|xs) 
        = OmaNonEmpty w where
            h = [f x | f <- g:fs, x <- y:xs]
            w = case h of (x:xs) -> x:|xs
-- IO
instance Applicative OmaIO where
    pure x = OmaIO (return x)
    OmaIO f <*> OmaIO x = OmaIO y where
        y = do
            f' <- f
            x' <- x
            return (f' x')
