{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Week2.Exercise2 where
import Data.Bifunctor
import Data.Functor.Contravariant
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Profunctor hiding (Star, Costar)
import Data.Void
import Week2.Exercise1






data Sum m n a = InL (m a) | InR (n a)
  deriving Show

instance (Functor m, Functor n) => Functor (Sum m n) where
   fmap f (InL x) = InL (fmap f x)
   fmap f (InR x) = InR (fmap f x)

{-
Todistus:
Lait pätevät, koska tässä hyödynnetään tiedettyjä funktoreita m ja n.
-}  



data Product m n a = Pair {fstPair :: m a, sndPair :: n a}
  deriving Show

instance (Functor m, Functor n) => Functor (Product m n) where
  fmap f (Pair {fstPair = x, sndPair = y }) 
    = Pair { fstPair = (fmap f x) , sndPair = (fmap f y) }
{-
Todistus:
Lait pätevät, koska tässä hyödynnetään tiedettyjä funktoreita m ja n.
-}  

newtype Identity a = Identity {runIdentity :: a}
  deriving Show

instance Functor Identity where
  fmap f (Identity {runIdentity = x }) = Identity {runIdentity = f x}

{-
Todistukset:
ID:
Pätee selvästi

f.g:
Tämäkin pätee selvästi.
fmap (f.g) (Identity {runIdentity = x }) = Identity {runIdentity = (f.g) x} ==
  (fmap f).(fmap g) (Identity {runIdentity = x })
-}  

newtype Compose m n a = Compose {getCompose :: m (n a)}
  deriving Show

instance (Functor m, Functor n) => Functor (Compose m n) where
  fmap f (Compose { getCompose = x }) = Compose { getCompose = fmap (fmap f) x }

{-
Todistukset:
Tässäkin hyödynnetään tunnettujen funktoreiden n ja m ominaisuuksia joten 
lait pätevät. m:ää siis fmap:ätään (fmap f):llä joka siis fmap:ää m:n sisältöä
eli n:ää.

-}

-- \(fmap f x) -> fmap f 
-- m funktori joten 
-- fmap f (m a) = m b ja b funktori joten fmap 
-- fmap f (fmap f x) 
newtype Const a b = Const {getConst :: a}
  deriving Show

instance Functor (Const a) where
  fmap f (Const {getConst = x}) = (Const {getConst = x})

{-
Lait pätevät selvästi.
-}

data Proxy a = Proxy
  deriving Show

instance Functor Proxy where
  fmap f Proxy = Proxy
{-
Lait pätevät selvästi.
-}

newtype State a b = State {runState :: a -> (b, a)}

instance Functor (State a) where 
  fmap f (State {runState = x}) = State {runState = (g . x)} where
    g (x, y) = ((f x), y)
{-
Todistukset:
ID:
fmap id (State {runState = x})  = State {runState = x)} , koska
nyt g on selvästi id.

f . g :
Vaikutukset näkyvät funktiossa g - yhdistetyn funktion fmap - ominaisuudet pätee 
selvästi.
-}

newtype Cont a b = Cont {runCont :: (b -> a) -> a}


instance Functor (Cont a) where
  fmap f (Cont {runCont = x}) = Cont {runCont = x . (. f)}
{-
ID:
Koska 
(. id ) :: ( b->c ) -> b -> c
, nähdään, että x yhdistetty siihen tulee samanlainen tyyppimuoto, kuin ilman
yhdistämistä eli x :: (b->c) -> c 
joten id pätee.


-}
-- (. f) :: (b -> r) -> a -> r
-- x :: (a -> r) -> r
-- x . (. f) :: (b -> r) -> r 

newtype Star m a b = Star {runStar :: a -> m b}

instance (Functor m) => Functor (Star m r)  where
  fmap f (Star x) = Star ((fmap f) . x )
{-
Todistukset:

m on funktori, ja hyödynnetään sen käyttäytymistä, joten lait pätevät.

-}

newtype Costar m a b = Costar {runCostar :: m a -> b}

instance Functor (Costar m a) where
  fmap f (Costar x) = Costar (f . x)
{-
Todistukset:

Todistettu Exercise1.hs :ssä Omafunktiolle.
-}

newtype Yoneda m a = Yoneda {runYoneda :: forall b. (a -> b) -> m b}

instance (Functor m) => Functor (Yoneda m) where
  fmap f (Yoneda {runYoneda = x}) = Yoneda {runYoneda = x . (. f)}
{-
Todistukset: 
ID: 
(. id) :: (a -> c) -> a -> c

Joten 
x . (. id ) :: (a -> c) -> m c  ja tämä täsmää pelkän x:n tyyppiin.

f.g : 
f :: c -> b
g :: a -> c
joten f . g :: a -> b

fmap (f.g) (Yoneda {runYoneda = x})  = Yoneda {runYoneda = x . (. (f . g))}

(. (f . g )) :: (b -> k) -> a -> k

(fmap f). (fmap g) (Yoneda {runYoneda = x}) = (fmap f) Yoneda {runYoneda = x . (. g)} =
   Yoneda {runYoneda = (x . (. g)) . (.f ) = 
     Yoneda {runYoneda = x . (. g) . (. f ) }

(. g) :: (c -> w) -> a -> w  
(. f) :: (b -> h) -> c -> h

Joten ((. g) . (. f))  :: (b -> h) -> a -> h

Tyyppitarkastelusta nähdään, että funktion (. (f . g )) tyyppi on sama kuin funktion
((. g) . (. f)), joten lain täytyy päteä.

-}
data Coyoneda m a = forall b. Coyoneda (b -> a) (m b)

instance (Functor m) => Functor (Coyoneda m) where
  fmap f (Coyoneda x y) = Coyoneda (f . x) y

{-
Todistukset:
Nämä pätevät selvästi. Samankaltainen tilanne on osoitettu
Exercise1.hs :ssä Omafunktiolle.

-}
