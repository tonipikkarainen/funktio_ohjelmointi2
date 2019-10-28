module Week1.Exerxise1 where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Void

newtype Bool' = Bool' {getBool' :: Bool}
  deriving Show

newtype Maybe' a = Maybe' {getMaybe' :: Maybe a}
  deriving Show

newtype Either' a b = Either' {getEither' :: Either a b}
  deriving Show

{--
Lait (decidable equivalence relation):
r (Reflexivity) : Kaikilla x A:sta -> x ~ x = True
s (Symmetry): Kaikilla x, y A:sta ->  x ~ y -> y ~ x
t (Transitivity): Kaikilla x, y , z A:sta ( x~y  ) /\ ( y~z) -> x~z
--}  

instance Eq Bool' where
    Bool' False == Bool' False  = True
    Bool' True  == Bool' True   = True
    _           == _            = False  

{--
Todistukset Bool' :n instansseille:
r (Reflexivity): selvästi x == x = True instanssin määritelmän perusteella
s (Symmetry): Kyllä toimii.
t (Transitivity): 
Vaihtoehdot -> 
1)    

x = Bool' True ja y = Bool' True -> x == y = True ja 
z = Bool' True -> y == z = True ja
silloin x == z = True 

2)
x = Bool' False ja y = Bool' False -> x == y = True ja 
z = Bool' False -> y == z = True ja
silloin x == z = True   

Eli x:n pitää olla sama kuin y:n ja y:n sama kuin z:n Tällöin myös
x on sama kuin z ja laki t toteutuu.
--}


instance Eq a => Eq (Maybe' a) where
    Maybe' (Just x) == Maybe' (Just y) = x == y
    Maybe' Nothing == Maybe' Nothing = True
    _ == _ = False
 
{- Todistukset Maybe':
r: Tämä on selvästi totta, kun a:lle on lain täyttävä Eq instanssi (ja tehtävässä a:lle
oli annettu instanssi).
s: Taas laki pätee selvästi, kun laki pätee a:lle.
t:
Tapaus 1. 
Tarkastellaan tapaus kun x, y ja z ovat Maybe' Nothing. 
Tällöin ( x~y  ) /\ ( y~z) ja myös  x~z.


Tapaus 2. 
Asetetaan x = Maybe' (Just x') ja y =  Maybe' (Just y') ja z = Maybe' (Just z').
Tällöin oletus x == y ja y == z johtaa tilanteeseen 
x' == y' ja y' == z' ja, koska a:lla on instanssi siitä seuraa että
x' == z', jolloin määritelmän mukaan myös
Maybe' (Just x') == Maybe' (Just z')

Jolloin transitiivisuus pätee 
-}