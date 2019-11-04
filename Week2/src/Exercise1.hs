module Week2.Exercise1 where
import Data.Bifunctor
import Data.Functor.Contravariant
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Profunctor
import Data.Void


newtype Endo' a = Endo' {appEndo' :: a -> a}

newtype Op' a b = Op' {getOp' :: b -> a}

newtype Maybe' a = Maybe' {getMaybe' :: Maybe a}
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
    -}

-- 1. Bool
-- Tämä ei toimi, sillä Bool tyypillä ei ole argumentteja, joten
-- kaikkien erilaisten funktoreiden funktioiden tyyppimäärittelyissä
-- esiintyvät "m a" ja "m b" jne eivät toimi. 

-- 2. Maybe a

instance  Functor Maybe' where
    fmap _  (Maybe' Nothing)   =  (Maybe' Nothing)
    fmap f (Maybe' (Just a))      =  Maybe' (Just (f a))


