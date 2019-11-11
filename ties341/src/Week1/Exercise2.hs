module Week1.Exercise2 where
-- Tekijä: Toni Pikkarainen
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Void
import Week1.Exercise1 -- otetaan omia wrappereita Exercise1:stä

newtype All = All {getAll :: Bool} 
    deriving Show
newtype Any = Any {getAny :: Bool} 
    deriving Show

-- 1. Bool : lle Monoidit All, Any 
instance Semigroup All where
    All x <>  All y = All (x && y)
instance Monoid All where
    mempty = All True 

instance Semigroup Any where
    Any x <>  Any y = Any (x || y)
instance Monoid Any where
    mempty = Any False  

-- Monoideja ovat lisäksi XNOR ja XOR, kirjoitan ne mikäli aikaa riittää.
-- Kuitenkin toteutukset ja todistukset ovat melkolailla samanlinjaiset.

{-Todistukset:
Diskreetti Semigroup All:
Exercise 1:n perusteella täyttyy ehto e kaikille Bool:eille.
p:  (x == y) /\ (z==w) -> x <> z == y <> w
Vaihtoehto 1: x = True ja y = True ja z = True ja w = True.
-> Tästä seuraa selvästi x <> z == y <> w
Vaihtoehto 2: x = False ja y = False ja z = False ja w = False.
-> Tästä seuraa selvästi x <> z == y <> w
Vaihtoehto 3: x = False ja y = False ja z = True ja w = True.
-> Tästä seuraa selvästi x <> z == y <> w
Vaihtoehto 4: symmetrinen vaihtoehto 3:n kanssa

a: x<> (y<>z) == (x<>y) <> z

Pätee selvästi sillä kolmesti arvosta tulee True vain jos kaikki True
ja False jos yksikin on False.

Diskreetti Monoid All:
Löytyy Diskreetti Semigroup instanssi.

l: mempty <> All True = All True ja mempty <> All False = All False
r: tämäkin pätee selvästi.

Diskreetti Semigroup Any: 
p: 
Pätee sillä, jos yksikin alkuperäisistä pareista on True - True
molemmat jälkimmäiset parit tuottavat True. Jos taas molemmat alkuperäiset
parit ovat False - False jälkimmäiset parit tuottavat molemmat Falset.

a: 
Pätee selvästi sillä tulos molemmilla puolilla True jos yksikin on True ja False
muuten.

Diskreetti Monoid All:
Löytyy Diskreetti Semigroup instanssi.

l & r: Molemmat ehdot toteutuvat -> Tässä l:n esimerkit:
mempty <> Any False = Any False
mempty <> Any True = Any True


-}
-- 2. 
instance Semigroup a => Semigroup (Maybe' a) where
    Maybe' (Just x) <> Maybe' (Just y) = Maybe' (Just (x <> y ))
    Maybe' (Just x) <> Maybe' (Nothing) = Maybe' (Just x)
    Maybe' (Nothing) <> Maybe' (Just y) = Maybe' (Just y )  
    Maybe' (Nothing) <> Maybe' (Nothing) = Maybe' (Nothing)

instance Semigroup a => Monoid (Maybe' a) where
    mempty = Maybe' Nothing

{-
Todistukset Maybe:
Diskreetti Semigroup:
Exercise 1:n perusteella täyttyy ehto e.
p:  (x == y) /\ (z==w) -> x <> z == y <> w

Vaihtoehdot:
x ja y = Maybe Nothing  ja z ja w = Maybe Nothing (1)
x = Maybe' (Just x') ja y = Maybe' (Just y') siten, että x' == y' = True
                        ja z ja w = Maybe' Nothing (2)
(3) on (2) :n toisinpäin
x = Maybe' (Just x') ja y = Maybe' (Just y') siten, että x' == y' = True
    a z = Maybe' (Just z') ja w = Maybe' (Just w') siten että z' == w' = True (4)

(4):
x <> z = Maybe' (Just (x' <> z' )) 
{ koska x' == y' = True ja z' == w' = True }
= Maybe' (Just (y' <> w' )) = y <> w
joten  
 x <> z == y <> w  on tosi.
 (1), (2) ja (3) toteutuvat selvästi määritelmien perusteella.

a: x<> (y<>z) == (x<>y) <> z

Pätee selvästi sillä määritelmä palautuu a:n määritelmään, joka
on Semigroup - kenties p:kin olisi voinut perustella tällä.

Diskreetti Monoid Maybe':
l & r : instanssien määritelmien perusteella nähdään, että
mempty on validi identiteettialkio operaatiolle..

-}

-- 3.
-- Tuottaa oikeanpuoleisen, jos vasen on Left.
-- Tuottaa vasemmanpuoleisen, jos vasen on Right.
-- Tämän voisi määritellä myös toisinpäin.   
instance Semigroup (Either' a b) where
    Either' (Left _) <> b = b 
    a  <> _ = a



{-
Todistukset 

diskreetti Semigroup Either':
Jos on instanssi (Eq a, Eq b) ja Eq (Either' a b) saadaan ehto
e täyttymään.

p:  (x == y) /\ (z==w) -> x <> z == y <> w
Jos ehto e pätee tämäkin pätee sillä, kun x == y niin x <> z ja y <> w 
tuottavat aina molemmat joko vasemman tai molemmat oikean alkion.
Kun vielä  z==w, niin  x <> z == y <> w.

a: x <> (y<>z) == (x<>y) <> z 
Tämäkin toimii: 
- jos x = Either' (Right..) molemmista jää vain x
- Jos x = Either' (Left ..) ja y = x = Either' (Right ..) molemmista jää vain y.
- Muuten molemmista jää z

Järkevää monoidia ei ole.

-}

-- 4.
instance (Semigroup a, Semigroup b) => Semigroup (OmaTuple a b) where
    OmaTuple (x,y) <> OmaTuple (z,w) = OmaTuple (x <> z, y <> w)

instance (Monoid a, Monoid b) => Monoid (OmaTuple a b) where
    mempty = OmaTuple (mempty,mempty)

{-
Todistukset:
Diskreetti Semigroup OmaTuple:
Exercise 1:n perusteella täyttyy ehto e.

p:  (x == y) /\ (z==w) -> x <> z == y <> w
Tämä pätee, koska a ja b ovat Semigroupeja määritelmässä käytetään a:n ja b:n
semigroup-operaatioita.

a:  x <> (y<>z) == ( x <> y ) <> z

Tässäkin vetoan a:n ja b:n Semigroup-instansseihin. Laki pätee niille
ja niiden operaatiota käytetään tuplen sisällä, joten
laki pätee tuplelle.

Diskreetti Monoid OmaTuple:
l & r: Tässäkin hyödynnetään a:n ja b:n Monoidi-instansseja, joten
lakien pätiessä niille, se pätee myös OmaTuplelle.

-}

-- 5. ja 6. emme voi määrittää funktiolle diskreettejä Semigroup
-- tai Monoidi-instansseja, koska niille ehto e ei täyty.

-- 7.

instance Semigroup Tyhja where
    _ <> _  = Tyhja ()

instance Monoid Tyhja where
    mempty = Tyhja ()

{-
Todistukset:
Diskreetti Semigroup Tyhja:
Exercise 1:n perusteella täyttyy ehto e.
p:  (x == y) /\ (z==w) -> x <> z == y <> w
Pätee selvästi, kun on vain yksi arvo kyseistä tyyppiä olemassa. 
Ja (<>) operaatio tuottaa aina kyseisen arvon.

a:  x <> (y<>z) == ( x <> y ) <> z 
Pätee selvästi, kun on vain yksi arvo kyseistä tyyppiä olemassa.
Ja (<>) operaatio tuottaa aina kyseisen arvon.

Diskreetti Monoidi Tyhja:
l & r: 
Pätee selvästi, kun on vain yksi arvo kyseistä tyyppiä olemassa.
Ja (<>) operaatio tuottaa aina kyseisen arvon.

-}

-- 8. [a]

instance Semigroup (Lista a) where
        Lista x <> Lista y = Lista (x ++ y)
instance Monoid (Lista a) where
        mempty  = Lista []
{-
Todistukset:
diskreetti Semigroup [a]:
Jos on instanssi Eq a ja Eq [a] saadaan ehto
e täyttymään.

p:  (x == y) /\ (z==w) -> x <> z == y <> w
Jos ehto e pätee tämäkin pätee. Kun x: ään lisätään z, siitä tulee sama
kuin jos y:n (joka on sama kuin x) lisätään w (joka on sama kuin z)

a: x <> (y<>z) == (x<>y) <> z 
Kyllä tämä toimii. (++) : n määritelmän mukaan
x ++ ( y++z ) = x ++ [y1,...ym,z1,...,zk]  = [x1,...,xn,y1,...,ym,z1,...,zk]
(x++y) ++ z   = [x1,...,xn,y1,...,ym] ++ z = [x1,...,xn,y1,...,ym,z1,...,zk]
eli x ++ (y ++ z) == (x ++ y) ++ z 

Diskreetti monoidi:

l & r : selvästi [] ++ [x1,...xn] = [x1,...xn] ja [x1,...xn] ++ [] = [x1,...xn]

-}

-- 9. 



instance Semigroup (OmaNonEmpty a) where
       OmaNonEmpty (a :| as) <> OmaNonEmpty (b :| bs) 
        = OmaNonEmpty  (a :| (as ++ b : bs))

{-
Todistukset:
Jos on instanssi Eq OmaNonEmpty saadaan ehto e toimimaan.
p:  (x == y) /\ (z==w) -> x <> z == y <> w
Jos ehto e pätee tämäkin pätee. Kun x: ään lisätään z, siitä tulee sama
kuin jos y:n (joka on sama kuin x) lisätään w (joka on sama kuin z)

a: x <> (y<>z) == (x<>y) <> z 
Yhdistämisoperaatio on hyvin samanlainen kuin listoille. Kun se pätee
listoille pätee se myös tälle NonEmptylle.

Järkevää Monoidia ei ole koska kyseessä on NonEmpty.


-}

-- 10.

instance Semigroup OmaVoid where
    a <> _ = a

{-
Todistukset:
Diskreetti Semigroup Void
Exercise 1:n perusteella täyttyy ehto e.

p:  (x == y) /\ (z==w) -> x <> z == y <> w
Eq instanssin ja Semigroup instanssin määrittelyjen perusteella
tämä toteutuu.

a:  x <> (y<>z) == ( x <> y ) <> z 
Eq instanssin ja Semigroup instanssin määrittelyjen perusteella
tämä toteutuu.

-}

-- 11. Tällekin löytyy base-kirjastosta monoidi ja semigroup.
-- Ei kuitenkaan ole ole Eq instanssia, joten ei saada lakia
-- e pätemään.

-- 12
-- Tähän en keksinyt vastausta.
-- Katsoin base-kirjaston instansseja Map:lle ja 
-- siellä oli vaadittu, että on (Ord k). En tiedä tämän
-- toteuttaisi, kun k:lle ei ole instansseja annettu.