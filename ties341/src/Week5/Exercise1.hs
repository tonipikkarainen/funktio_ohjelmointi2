module Week5.Exercise1 where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map (..))
import qualified Data.Map as Map
import Week2.Exercise1
import Week3.Exercise1

-- Todistukset puuttuvat - käytin aikaa
-- muihin tehtäviin.

instance Foldable Maybe' where
    foldMap f (Maybe' (Just x)) = f x
    foldMap f (Maybe' Nothing) = mempty

instance Traversable Maybe' where
    sequenceA (Maybe' (Just x)) = fmap (\y -> Maybe' (Just y)) x
    sequenceA (Maybe' (Nothing)) = pure (Maybe' Nothing)
--

instance Foldable (Either' a) where 
    foldMap f (Either' (Right x) ) = f x
    foldMap f ((Either' (Left x) )) = mempty

instance Traversable (Either' a) where
    sequenceA (Either' (Right x)) = 
        fmap (\y -> Either' (Right y)) x
    sequenceA (Either' (Left x)) = pure (Either' (Left x))

--

instance Foldable (OmaTuple a) where
    foldMap f (OmaTuple (x,y)) = f y

instance Traversable (OmaTuple a) where
    sequenceA (OmaTuple (x,y)) = fmap (\z -> OmaTuple (x,z)) y

--

instance Foldable Lista where
    foldMap f (Lista x) = mconcat (map f x)

instance Traversable Lista where
    sequenceA (Lista []) = pure (Lista [])
    sequenceA (Lista (x:xs)) = f <$> x <*> (sequenceA (Lista xs)) where
        f y (Lista x) =  Lista (y:x)
--

instance Foldable OmaNonEmpty where
    foldMap f (OmaNonEmpty (x :| xs)) = f x <> (foldMap f (Lista xs))

instance Traversable OmaNonEmpty where
    sequenceA (OmaNonEmpty (x :| [])) = f <$> x <*> pure (Lista []) where
        f y (Lista x) = OmaNonEmpty (y :| x )
    sequenceA (OmaNonEmpty (x :| (z:zs))) = g <$> x <*> (sequenceA (Lista (z:zs))) where
        g x (Lista xs) = OmaNonEmpty (x :| xs)


-- 

instance Foldable (OmaMap k) where
    foldMap f (OmaMap x) = foldMap f (Lista (Map.elems x))

instance (Ord k) => Traversable (OmaMap k) where
    sequenceA (OmaMap x) = uusiOmaMap  where
        z = Map.elems x
        lista = sequenceA z
        keys = Map.keys x
        uusi = fmap (zip keys) lista
        uusi_map = fmap (Map.fromList) uusi
        uusiOmaMap = fmap (OmaMap) uusi_map



    