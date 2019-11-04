module Exercise1 where
-- Tekijä: Toni Pikkarainen
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

newtype OmaTuple a b = OmaTuple {getTuple :: (a,b)} 
  deriving Show

newtype Tyhja  = Tyhja {getTyhja :: () }  
  deriving Show

newtype Lista a = Lista {getLista :: [a]}
  deriving Show

newtype OmaNonEmpty a = OmaNonEmpty {getEmpty :: NonEmpty a}
  deriving Show

newtype OmaVoid = OmaVoid {getVoid :: Void}

newtype OmaMap k a = OmaMap {getMap :: Map k a}
  deriving Show
{--
Lait (decidable equivalence relation):
r (Reflexivity) : Kaikilla x A:sta -> x ~ x = True
s (Symmetry): Kaikilla x, y A:sta ->  x ~ y -> y ~ x
t (Transitivity): Kaikilla x, y , z A:sta ( x~y  ) /\ ( y~z) -> x~z
--}  
-- 1.
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

-- 2.
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

-- 3.
instance (Eq a, Eq b) => Eq (Either' a b) where
  Either' (Left x)  == Either' (Left y) = x == y
  Either' (Right x) == Either' (Right y) = x == y
  _                 == _  = False 

{- Todistukset Either':
r: Tämä on selvästi totta, kun sama pätee a:lle ja b:lle.
s: Taas laki pätee selvästi, kun laki pätee a:lle ja b:lle.
t:
Tapaus 1. 
x = Either' (Left x'), y = Either' (Left y') ja z = Either' (Left z')
Oletuksesta (( x~y  ) /\ ( y~z)) ja instanssin määrittelystä seuraa -> 
x' == z' = True  , kun a:lle ja b:lle on instanssit. Tällöin instanssin
määrittelyn perusteella totta on myös 
x == z .

Sama perustelu voitaisiin esittää myös Right - konstruktorilla muodostetuille
arvoille vaihtamalla Left - sanan paikalle Right. 

Transitiivisuus pätee. 
-}

-- 4.

instance (Eq a, Eq b) => Eq (OmaTuple a b) where
  OmaTuple (x,y) == OmaTuple (z,w) = (x == z && y == w)
  
{- Todistukset OmaTuple:
r: Tämä on selvästi totta, kun sama pätee a:lle ja b:lle.
s: Taas laki pätee selvästi, kun laki pätee a:lle ja b:lle.
t:

x = OmaTuple (x',x'')
y = OmaTuple (y',y'')
z = OmaTuple (z',z'')

Oletus ja määritelmät -> (x' == y' && x'' == y'') = True ja 
                          (y' == z' && y'' == z'') = True 

Taas pätee selvästi (x' == z' && x'' == z'' ) = True 
a:n ja b:n instanssien perusteella. Ja siis myös pätee x == z = True                           

Riittäisiköhän tähänkin kohtaan vaan perusteluksi, että
määritelmän ja a:n ja b:n instanssien perusteella pätee?

Transitiivisuus pätee. 
-}

-- 5. ja 6. Funktioiden vertaaminen ei onnistu. Voisiko tässä käyttää
-- samaa perustelua, kuin muistiinpanoissa kahden ohjelman vertaamisen
-- tilanteessa? Eli Jos halutaan verrata kahta funktiota, mutta toinen
-- ei pysähdy vertaaminen ei onnistu.

-- 7.

instance Eq Tyhja where
  _ == _ = True
{-
Koska () tyyppi sisältää ainoastaan yhden arvon, niin sitä
verrattaessa saman tyyppiseen arvoon täytyy aina palauttaa True.

Tällä kaikki lait pätevät selvästi.

-}

-- 8.
instance Eq a => Eq (Lista a) where
  Lista [] == Lista [] = True
  Lista (x:xs) == Lista (y:ys) = x == y && Lista xs == Lista ys
  _ == _ = False
  --vertaa xs ys where
    --vertaa [] [] = True 
    --vertaa (x:hs) (y:zs) 
    --  | length (x:hs) /= length (y:zs) = False
    --  | otherwise                      = x == y && (vertaa hs zs)

{- Todistukset Lista:
r: Tämä on selvästi totta (Eq a):n perusteella .
s: Taas laki pätee selvästi, kun laki pätee a:lle.
t:
Tässäkin varmaan riittää sanoa, että transitiivisuus pätee
koska se pätee myös a:lle ja lopulta tässä vertaillaan
listoja alkioittain. Kun listan vertailu on alkioittain transitiivista
pätee transitiivisuus myös listoille.

Transitiivisuus pätee. 
-}

-- 9.
-- Hyödynnetään oman Listan vertailua
instance Eq a => Eq (OmaNonEmpty a) where
  OmaNonEmpty x == OmaNonEmpty y = 
    vertaa x y where
      vertaa (z :| zs) (h :| hs) 
        | length (z :| zs) /= length (h :| hs) = False
        | otherwise                     = z == h && (Lista zs == Lista hs)

{- Todistukset OmaNonEmpty:

Tässä kaikki kohdat pätevät sen vuoksi, että ne pätevät 
a:lle (ensimmäisten alkioiden vertailu) ja Listalle.

Transitiivisuus pätee. 
-}

-- 10.
-- 
instance Eq OmaVoid where
  _  == _  = True

-- Void:lle on olemassa instanssi, joten päättelin
-- että tällainen pitää pystyä tekemään.
-- Tälle instanssille pätevät selvästi kaikki vaaditut lait

-- 11.

-- IO ei ole osa Eq tyyppiluokkaa.

-- 12. Map k a
instance Eq a => Eq (OmaMap k a) where
  (OmaMap m1) == (OmaMap m2) = 
    Lista (Map.elems m1) == Lista (Map.elems m2)
-- Tässä ei voida tarkistaa avaimia, koska niiden instanssia
-- ei ole käytössä. 
-- Tämä siis voi palauttaa True Map:eille, joissa on 
-- erilaiset avaimet, mutta samat elementit samassa avainten 
-- määrittämässä järjestyksessä

{- Todistukset Lista:
r: Tämä on selvästi tosi.
s: Tämäkin on tosi sillä järjestyksen vaihtaminen ei vaikuta
määritelmään totuuteen.

t: Tämäkin on tosi, koska transitiivisuus pätee Listalle.

-}



