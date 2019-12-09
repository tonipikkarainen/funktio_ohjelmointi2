{-# LANGUAGE ViewPatterns #-}

module Week6.Exercise2 where

class Covariant m where
  (<&>) :: m a -> (a -> b) -> m b
infixl 1 <&>

class Covariant m => Monoidal m where
  unit :: m ()
  (>*<) :: m a -> m b -> m (a, b)
infix 5 >*<

class Monoidal m => Triad m where
  join :: m (m a) -> m a

newtype WrappedFunctor m a = WrapFunctor {unwrapFunctor :: m a}
-- (1)
instance Covariant m => Functor (WrappedFunctor m) where
  fmap f (WrapFunctor xs) = WrapFunctor (xs <&> f) -- itse 
-- Todistus - Funktorilait
{-
-- prop> fmap id = id:

   fmap id (WrapFunctor xs)
== WrapFunctor (xs <&> id) 
{- covariant - laki - 1 -}
== WrapFunctor ( xs ) == id  (WrapFunctor xs )

-- prop> fmap (g . f) = fmap g . fmap f

   fmap (g . f) (WrapFunctor xs)
== WrapFunctor (xs <&> (g . f)) 
{- covariant - laki - 2 -}
== WrapFunctor (xs <&> f <&> g)

   fmap g . fmap f (WrapFunctor xs)
== fmap g WrapFunctor (xs <&> f)
== WrapFunctor ((xs <&> f) <&> g )
{- koska operaatio <&> on vasemmalle assosiatiivinen -}
== WrapFunctor (xs <&> f <&> g)

Funktorilait pätevät.

-}
 -- (2)
instance Functor m => Covariant (WrappedFunctor m) where
  WrapFunctor xs <&> f = WrapFunctor (fmap f xs) -- itse
-- Todistus covariant-lait:
{-
-- prop> xs <&> id = xs
pätee selvästi, koska funktorilla on samankaltainen id - ominaisuus.

-- prop> xs <&> g . f = xs <&> f <&> g

   WrapFunctor xs <&> g . f  
== WrapFunctor (fmap (g . f) xs)
{- Funktorilaki 2 -}
== WrapFunctor (fmap g . fmap f xs)

   (WrapFunctor xs) <&> f <&> g
== WrapFunctor (fmap f xs) <&> g
== WrapFunctor (fmap g ( fmap f xs)) 
== WrapFunctor (fmap g . fmap f xs)

Covariant - lait pätevät.
-}



newtype WrappedCovariant m a = WrapCovariant {unwrapCovariant :: m a}

instance Covariant m => Functor (WrappedCovariant m) where
  fmap f (WrapCovariant xs) = WrapCovariant
    (unwrapFunctor (fmap f (WrapFunctor xs))) -- tämä todistettu kohdassa (2)

instance Functor m => Covariant (WrappedCovariant m) where
  WrapCovariant xs <&> f = WrapCovariant (unwrapFunctor (WrapFunctor xs <&> f)) 
---- tämä todistettu kohdassa (1)

newtype WrappedApplicative m a = WrapApplicative {unwrapApplicative :: m a}

instance Covariant m => Functor (WrappedApplicative m) where
  fmap f (WrapApplicative xs) = WrapApplicative
    (unwrapFunctor (fmap f (WrapFunctor xs))) -- tämä todistettu kohdassa (2)

instance Functor m => Covariant (WrappedApplicative m) where
  WrapApplicative xs <&> f = WrapApplicative
    (unwrapFunctor (WrapFunctor xs <&> f)) ---- tämä todistettu kohdassa (1)

-- (4)
instance Monoidal m => Applicative (WrappedApplicative m) where
  pure x = WrapApplicative (unit <&> (\() -> x)) -- itse
  WrapApplicative fs <*> WrapApplicative xs = WrapApplicative (unwrapFunctor $
                        fmap (\x -> (fst x) (snd x) )  (WrapFunctor (fs >*< xs))) -- itse
-- Todistus applicative - lait:
{- 
Nämä tiedetään:


prop> pure id <*> xs = xs:

WrapApplicative fs == pure id
WrapApplicative fs <*> WrapApplicative xs ( fs :: m (a->a) xs :: a)
{-  pure id:n rakenteen sisällä todella on id funktio johtuen määritettelystä (unit <&> (\() -> x)) -}
{- (WrapFunctor (fs >*< xs) - : WrapFunctor (id, xs) -}
{- fmap (\x -> (fst x) (snd x) ) : id xs = xs -}
== WrapApplicative (unwrapFunctor (WrapFunctor (xs))) 
== WrapApplicative (xs) 

-- prop> pure (.) <*> fs <*> gs <*> xs = fs <*> (gs <*> xs):

-- jätetään kääreet välistä, jotta kirjoitustyö helpottuu hiukan.
   fs <*> (gs <*> xs)
== fs <*> (fmap (\x -> (fst x) (snd x) ) (gs >*< xs)))

--  Tuossa operaattorin oikea puoli sellainen, missä
--  gs:n sisällä olevaa funktiota on sovellettu argumenttinaan xs:n sisältö.
--  Jälkimmäinen operaatio tuottaa sen fs:n sisältöä sovelletaan edelliseen. 
--  Jos fs :: m (b -> c ) ja  gs :: m ( a -> b ) ja xs :: m a,
--  lopputuloksen tyyppi on m c             (*)


    pure (.) <*> fs <*> gs <*> xs  
==  (fmap (\x -> (fst x) (snd x) ) ((.) >*< fs))) <*> gs <*> xs 

-- Tässä ensin parista "pure (.) <*> fs" muodostuu osittainen yhdistetty funktio,
-- jossa fs on se jota tullaan soveltamaan viimeisenä.
-- Seuraavaksi muodostuu (fs . gs), jota sitten sovelletaan lopulta xs:ään
-- mikä on oleellisesti sama asia kuin tilanteessa  (*)

-- prop> pure f <*> pure x = pure (f x):

Edellä on todettu, että pure nostaa tyypin
halutun rakenteen sisään.

Lisäksi on todettu, että operaatio (<*>) soveltaa ensimmäiseen argumentin sisällä
olevaa funktiota toisen argumentin sisällä olevaan tyyppiin.

Siispä on sama nostetaanko funktio ja tyyppi ensin  halutun rakenteen sisälle
ja sovelletaan funktiota sitten vai sovelletaanko funktiota ensin
ja nostetaan sitten tulos halutun rakenteen sisälle.

-- prop> fs <*> pure x = pure ($ x) <*> fs:

Edellä on todettu, että pure nostaa sisältönsä halutun
rakenteen sisälle. Yhtäsuuruus merkin vasemmalla puolella
fs:n sisältö sovelletaan x:ään. Yhtäsuuruus merkin oikealla puolella
($ x):ää sovelletaan fs:än sisältöön. Koska ($ x) ottaa argumentikseen
funktion, jota sitten sovelletaan x:ään ovat puolet yhtä suuret.


Applicative - lait pätevät.

-}
-- \x y -> (x,y)
-- (5)
instance Applicative m => Monoidal (WrappedApplicative m) where
  unit = WrapApplicative (pure ()) -- itse
  WrapApplicative xs >*< WrapApplicative ys = WrapApplicative ((\x y -> (x,y)) <$> xs <*> ys) -- itse

-- Todista monoidal-lait: 

{-
-- prop> let lu = snd in lu <$> unit >*< ys = ys

Pätee selvästi, sillä määrittelystä sovelletaan tuplen tekevää funktiota 
applikatiivisesti xs:ään ja ys:ään.

-- prop> let ru = fst in ru <$> xs >*< unit = xs

Pätee selvästi, sillä määrittelystä sovelletaan tuplen tekevää funktiota 
applikatiivisesti xs:ään ja ys:ään.

-- prop> let a (x, (y, z)) = ((x, y), z) in a <$> xs >*< (ys >*< zs) = (xs >*< ys) >*< zs

- xs >*< (ys >*< zs) :n tyyppi on määritelmän myötä muotoa m (a , (b , c ))
- kun tuohon sovelletaan (fmap a) a muuttaa sisältöä muotoon: m ((a , b ), c )
- ja lopputulos on sama kuin yhtäsuuruusmerkin oikealla puolella.


Monoidal-lait pätevät.

-}
newtype WrappedMonoidal m a = WrapMonoidal {unwrapMonoidal :: m a}

instance Covariant m => Functor (WrappedMonoidal m) where
  fmap f (WrapMonoidal xs) = WrapMonoidal
    (unwrapFunctor (fmap f (WrapFunctor xs))) --tämä todistettu kohdassa (1)

instance Functor m => Covariant (WrappedMonoidal m) where
  WrapMonoidal xs <&> f = WrapMonoidal (unwrapFunctor (WrapFunctor xs <&> f)) -- tämä todistettu kohdassa (2)

instance Monoidal m => Applicative (WrappedMonoidal m) where
  pure x = WrapMonoidal (unwrapApplicative (pure x))
  WrapMonoidal fs <*> WrapMonoidal xs = WrapMonoidal
    (unwrapApplicative (WrapApplicative fs <*> WrapApplicative xs)) --tämä todistettu kohdassa (4)

instance Applicative m => Monoidal (WrappedMonoidal m) where
  unit = WrapMonoidal (unwrapApplicative unit)
  WrapMonoidal xs >*< WrapMonoidal ys = WrapMonoidal
    (unwrapApplicative (WrapApplicative xs >*< WrapApplicative ys)) -- tämä todistettu kohdassa (5)

newtype WrappedMonad m a = WrapMonad {unwrapMonad :: m a}

instance Covariant m => Functor (WrappedMonad m) where
  fmap f (WrapMonad xs) = WrapMonad (unwrapFunctor (fmap f (WrapFunctor xs))) -- tämä todistettu kohdassa (1)

instance Functor m => Covariant (WrappedMonad m) where
  WrapMonad xs <&> f = WrapMonad (unwrapFunctor (WrapFunctor xs <&> f)) -- tämä todistettu kohdassa (2)

instance Monoidal m => Applicative (WrappedMonad m) where
  pure x = WrapMonad (unwrapApplicative (pure x))
  WrapMonad fs <*> WrapMonad xs = WrapMonad
    (unwrapApplicative (WrapApplicative fs <*> WrapApplicative xs)) -- tämä todistettu kohdassa (4)

instance Applicative m => Monoidal (WrappedMonad m) where
  unit = WrapMonad (unwrapApplicative unit)
  WrapMonad xs >*< WrapMonad ys = WrapMonad
    (unwrapApplicative (WrapApplicative xs >*< WrapApplicative ys)) -- tämä todistettu kohdassa (5)

-- mikä on tuo nuoli?

-- (6)
instance Triad m => Monad (WrappedMonad m) where
  WrapMonad xs >>= ((unwrapMonad .) -> k) = WrapMonad (join (xs <&> k)) -- itse
-- Todistus monad-lait:

{-


-- prop> pure x >>= k = k x
k :: a -> m b

(>>=):n määrittelyssä k:ta sovelletaan xs:ään covariantin fmap:ia vastaavalla funktiolla
ja poistetaan ylimääräinen kuori joinilla - tämä on sama kuin jos sovellettaisiin k:ta
suoraan xs:n sisältöön - eli 
pure x >>= k = k x 
pätee.

-- prop> xs >>= pure = xs

Nyt pure on bindissa käytettävä funktio. Sillä map:ätään xs:n sisälle ja 
poistetaan ylimääräinen kuori joinilla - tämä on sama asia kuin pelkkä xs.

-- prop> xs >>= (\ x -> k x >>= m) = xs >>= k >>= m

Määritelmästä näkyy selvästi, että ketjutus toimii - bind:in oikean puolen 
funktiolla map:ätään rakenteen sisään ja joinilla poistetaan kuori - tämä voidaan
sitten syöttää seuraavalle funktiolle.
-}

-- (7)
instance Monad m => Triad (WrappedMonad m) where
  join (WrapMonad xs) = WrapMonad (xs >>= unwrapMonad) -- itse
-- Todistus Triad - lait:
-- prop> pure x >>= k = k x
-- prop> xs >>= pure = xs
-- prop> xs >>= (\ x -> k x >>= m) = xs >>= k >>= m
{-

-- prop> join . pure = id
Määritelmästä nähdään että join tekee selvästi, mitä halutaan:
xs :: m (WrappedMonad m a)
ja xs >>= unwrapMonad :: m a  (eli käytännössä bindissa vain kuoritaan rakennetta.)

Siispä join . pure = id, koska pure luo rakenteen ympärille ja join kuorii sen pois.

-- prop> join . fmap pure = id

Koska 
fmap pure :: m a -> m (m a) 
ja
join :: m (m a) -> 
ja kumpikaan, pure eikä join, eivät muuta arvoja vain pelkästään kuorivat rakenteen tai rakentevat
sisällön ympärille uuden rakenteen väite on selvästi totta.

-- prop> join . fmap join = join . join


Yhtälön oikea ja vasen puoli ovat yhtä suuret, koska molemmissa tarvitaan kaksi kerrosta,
jotka voidaan kuoria ( m (m (m a))) ja ensimmäisen tapauksessa kuoriminen aloitetaan 
vain sisäpuolelta. Koska join ei mitenkään muuta sisältöä, on sama kumpi kerros kuoritaan
ensin.



-}

newtype WrappedTriad m a = WrapTriad {unwrapTriad :: m a}

instance Covariant m => Functor (WrappedTriad m) where
  fmap f (WrapTriad xs) = WrapTriad (unwrapFunctor (fmap f (WrapFunctor xs))) --tämä todistettu kohdassa (1)

instance Functor m => Covariant (WrappedTriad m) where
  WrapTriad xs <&> f = WrapTriad (unwrapFunctor (WrapFunctor xs <&> f)) -- tämä todistettu kohdassa (2)

instance Monoidal m => Applicative (WrappedTriad m) where -- tämä todistettu kohdassa (4)
  pure x = WrapTriad (unwrapApplicative (pure x))
  WrapTriad fs <*> WrapTriad xs = WrapTriad
    (unwrapApplicative (WrapApplicative fs <*> WrapApplicative xs))

instance Applicative m => Monoidal (WrappedTriad m) where -- tämä todistettu kohdassa (5)
  unit = WrapTriad (unwrapApplicative unit)
  WrapTriad xs >*< WrapTriad ys = WrapTriad
    (unwrapApplicative (WrapApplicative xs >*< WrapApplicative ys))

instance Triad m => Monad (WrappedTriad m) where -- tämä todistettu kohdassa (6)
  WrapTriad xs >>= ((unwrapTriad .) -> k) = WrapTriad
    (unwrapMonad (WrapMonad xs >>= WrapMonad . k))

instance Monad m => Triad (WrappedTriad m) where -- tämä todistettu kohdassa (7)
  join ((<&> unwrapTriad) -> WrapTriad xs) = WrapTriad
    (unwrapMonad (join (WrapMonad (fmap WrapMonad xs))))