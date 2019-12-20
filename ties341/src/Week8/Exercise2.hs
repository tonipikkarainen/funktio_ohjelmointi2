module Week8.Exercise2 where

import Control.Exception
import Control.Monad.Catch
import Data.Stream.Infinite (Stream (..))
import qualified Data.Tree as Rose (Forest (..), Tree (..))
import Week8.Exercise1

-- MonadThrow m => A -> m ZA

-- puuttuu: 
-- * builderi rosetreelle
-- * liikkuminen 
--      * stream 
--      * rosetree 

data MyException = OutOfTreeException | OutOfListException
    deriving Show

instance Exception MyException

-- Maybe
makeDMaybe :: MonadThrow m => Maybe a -> m (DMaybe a)
makeDMaybe x = pure DMaybe
-- Liikkuminen on käytännössä id - funktio.

-- Join 
makeDJoinL ::  MonadThrow m => (a,a) -> m (DJoin a)
makeDJoinL (x, y) = pure (Left x )

makeDJoinR ::  MonadThrow m => (a,a) -> m (DJoin a)
makeDJoinR (x, y) = pure ( Right y )

-- Näille voitaisiin tehdä vasemman tai oikean
-- tuottavat funktiot

-- List
makeDList :: MonadThrow m => [a] -> m (DList a)
makeDList xs = pure ([],xs)

-- Liikkuminen lista-zipperissä
goLeft :: MonadThrow m => DList a -> m (DList a)
goLeft ([], xs) = throwM OutOfListException
goLeft (x:xs, ys ) = pure (xs, x:ys)

goRight ::  MonadThrow m => DList a -> m (DList a)
goRight (xs, []) = throwM OutOfListException
goRight (xs, y:ys) = pure (y:xs, ys)

-- Stream
makeDStream :: MonadThrow m => Stream a -> m (StreamD a)
makeDStream (x :> xs) = pure (x :> xs, []) 

-- Tree
-- type DTree a = ((Tree a, Tree a) , [(Bool, a, Tree a)]) 
-- data Tree a = Leaf | Node (Tree a) a (Tree a)

-- Tässä mietin, kuinka mallintaa kokonaisen puun muuntaminen
-- zipperiksi.
-- Totesin, että alkutilanteessa historiassa voidaan
-- ajatella olevan lehti - eli ei mitään.
-- Ja silloin on oikeastaan sama, onko
-- historiassa True vai False.
-- Kun kirjoitetaan liikkumisfunktioita
-- voidaan todeta, että
-- Jos historiassa on pelkkä
-- Leaf, ollaan top - solmussa.
makeDTree :: MonadThrow m =>  Tree a -> m (DTree a)
makeDTree x = case x of
    Leaf        ->  pure ((Leaf, Leaf),[])
    Node t n t' ->  pure ((t,t'), (True, n, Leaf):[]) 

-- Binarytree left
-- Left - False
turnLeftB :: MonadThrow m => (DTree a) -> m (DTree a)
turnLeftB ((Leaf,t'), xs )   = throwM OutOfTreeException
turnLeftB (((Node subL n subR) ,t'), xs ) =
     pure ( (subL, subR), (False, n, t'):xs )

-- Binarytree right     
-- Right - True
turnRightB :: MonadThrow m => (DTree a) -> m (DTree a)
turnRightB ((t,Leaf), xs )   = throwM OutOfTreeException
turnRightB ((t , (Node subL n subR)), xs ) =
     pure ( (subL, subR), (True, n, t):xs )

-- Binarytree back
goBackB ::  MonadThrow m => (DTree a) -> m (DTree a)
goBackB ( (t,t'), (x, y, Leaf):[] )   =  throwM OutOfTreeException
goBackB ( (t, t'), (x, y, t'') : xs ) = case x of
    False -> pure (((Node t y t'), t''), xs)
    True  -> pure ((t'', (Node t y t')), xs)


