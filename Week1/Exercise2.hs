module Exercise2 where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Void
import Exercise1

--class Eq a => Semigroup' a where
--    (<*>) :: a -> a -> a
--
--
--class Semigroup' a => Monoid' a where
--    mempty :: a

{-
1. Instances for Bool. Ei ole monoidi
2. Instances for Maybe a, given instances for a. On semigroup ja monoidi
3. Instances for Either a b. On semigroup
4. Instances for (a, b), given instances for a and b. On molemmat
5. Instances for a -> a. Ei, koska ei ole Eq:ta
6. Instances for a -> b, given instances for b. Ei, koska ei ole Eq:ta
7. Instances for ().
8. Instances for [a]. On
9. Instances for NonEmpty a.
10. Instances for Void.
11. Instances for IO a, given instances for a. Ei, koska ei ole Eq:ta
12. Instances for Map k a, given instances for a.
-}

-- 1. Bool : lle ei ole yhtä järkevää yhdistämis operaatiota, joten ei
-- ei semigroup eikä siten myöskään monoidi.

-- 2. 
instance Semigroup a => Semigroup (Maybe' a) where
    Maybe' (Just x) <> Maybe' (Just y) = Maybe' (Just (x <> y ))
    Maybe' (Just x) <> Maybe' (Nothing) = Maybe' (Just x)
    Maybe' (Nothing) <> Maybe' (Just y) = Maybe' (Just y )  
    Maybe' (Nothing) <> Maybe' (Nothing) = Maybe' (Nothing)

instance Semigroup a => Monoid (Maybe' a) where
    mempty = Maybe' Nothing
