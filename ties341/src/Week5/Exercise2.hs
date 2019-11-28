{-# LANGUAGE ConstraintKinds #-}

module Week5.Exercise2 where

import Data.Int (Int8 (..), Int16 (..))
import Data.Set (Set (..))
import qualified Data.Set as Set
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader as Reader
import Control.Monad.Trans.State.Strict as State

type Intlike a = (Bounded a, Integral a) 
{-
The program is composed from the Except Problem monad, the Reader
(Maybe Int) monad and the State (Set a) monad, whose associated
transformers can be stacked in six different ways, two of
which are given.  Figure out what the different stacks do, decide
which one makes the most sense for this application and write a 
helper function that runs the stack and returns the result.
-}
count :: Intlike a => a -> a
count n = 1 + n

safeToCount :: Intlike a => a -> Bool
safeToCount n = n < maxBound

collatz :: Intlike a => a -> a
collatz n
  | even n = n `div` 2
  | otherwise = 1 + 3 * n

safeToCollatz :: Intlike a => a -> Bool
safeToCollatz n
  | even n = True
  | otherwise = n > minBound `div` 3 && n < maxBound `div` 3

collatzBound :: Intlike a => Maybe a
collatzBound = Just 18

data Variable = Value | Counter | Cache
  deriving Show

data Problem = Loop | Bound Variable
  deriving Show
-- Tämä tuottaa:  :: (Bounded a, Integral a) => (Either Problem a, Set Int)
-- Parempi, sillä tuotokseen jää myös joukko eli tila.
checkCollatzSRE :: (Intlike a, Intlike b) =>
  a -> ExceptT Problem (ReaderT (Maybe Int) (State (Set a))) b
checkCollatzSRE = let
  f :: (Intlike a, Intlike b) =>
    a -> ExceptT Problem (ReaderT (Maybe Int) (State (Set a))) b
  f n = do
    ps <- (lift . lift) get                         -- otetaan tila statesta - eli joukko - alussa tyhjä n = syötetty numero
    if Set.member n ps then                         -- jos syöte on joukossa
      throwE Loop else do                           -- heitetään loop virhe
      ms <- lift ask                                -- ask - on readerin operaatio, pitää nostaa vain kerran
      case ms of                                    -- nyt ms :: Maybe Int
        Nothing -> if Set.size ps >= maxBound then  -- jos joukon koko on suurempi kuin maxBound heitetään throwE (Bound cache) :: on exceptT:n funktio
          throwE (Bound Cache) else                 
          (lift . lift) (put (Set.insert n ps))     -- muuten asetetaan tilaksi ((Set.insert n ps) lisätään joukkoon ps n)
        Just m -> if Set.size ps >= m then          -- Just m , jos joukon koko on suurempi tai yhtä suuri kuin m  -> pure () -> mihin asettaa tyhjän ? 
          pure () else  -- minkä tason arvo tämä on?                            -- asettaa tulokseksi ei mitään, ei lisää tulokseen mitään, ei myöskään kasvata joukkoa.
          (lift . lift) (put (Set.insert n ps))     -- kasvattaa joukkoa - Staten operaatio, pitää nostaa kaksi kertaa
      if abs n == 1 then                            -- jos n on yksi
        pure 0 else do                              -- tulokseksi asetetaan 0
        if not (safeToCollatz n) then               -- jos ei katsotaan onko turvallista suorittaa collatz
          throwE (Bound Value) else let             -- 
          p = collatz n in do                       -- jos on suoritetaan collatz funktio
          q <- f p                                  -- rekursio, suoritetaan f uudestaan p:lle - jos askelten lukumäärä sopiva -> 
          if not (safeToCount q) then               -- tulos on (q + 1)
            throwE (Bound Counter) else            
            pure (count q) in                       
  \ n -> catchE (f n) $ \ e -> case e of
    Bound Cache -> mapExceptT (local (const collatzBound)) (f n)
    _ -> throwE e
-- Tämä tuottaa:
-- :: (Bounded a, Integral a) => Either Problem (a, Set Int)
checkCollatzERS :: (Intlike a, Intlike b) =>
  a -> StateT (Set a) (ReaderT (Maybe Int) (Except Problem)) b
checkCollatzERS = let
  f :: (Intlike a, Intlike b) =>
    a -> StateT (Set a) (ReaderT (Maybe Int) (Except Problem)) b
  f n = do
    ps <- get
    if Set.member n ps then
      (lift . lift) (throwE Loop) else do
      ms <- lift ask
      case ms of
        Nothing -> if Set.size ps >= maxBound then
          (lift . lift) (throwE (Bound Cache)) else
          put (Set.insert n ps)
        Just m -> if Set.size ps >= m then
          pure () else
          put (Set.insert n ps)
      if abs n == 1 then
        pure 0 else
        if not (safeToCollatz n) then
          (lift . lift) (throwE (Bound Value)) else let
          p = collatz n in do
          q <- f p
          if not (safeToCount q) then
            (lift . lift) (throwE (Bound Counter)) else
            pure (count q) in
  \ n -> (State.liftCatch . Reader.liftCatch) catchE (f n) $ \ e -> case e of
    Bound Cache -> mapStateT (local (const collatzBound)) (f n)
    _ -> (lift . lift) (throwE e)

type InputInt = Int16    

runCheckCollatz x = runState ((runReaderT (runExceptT (g x)) ) (Just maxBound)) Set.empty where
  g :: InputInt -> ExceptT Problem (ReaderT (Maybe Int) (State (Set InputInt))) Int
  g =  checkCollatzSRE


-- lift :: Monad m => m a -> t m a
-- Except: 
-- newtype ExceptT e m a = ExceptT (m (Either e a))	
-- runExceptT :: ExceptT e m a -> m (Either e a)

-- Reader:
-- constructror: ReaderT (r -> m a)	
-- runReaderT :: ReaderT r m a -> r -> m a
-- ask :: m r (?)
-- Example: Reader [(String,Value)] a

-- StateT a = StateT {runStateT :: s -> m (a, s)	}
-- evalStateT :: Monad m => StateT s m a -> s -> m a
--runExcept (runReaderT(runStateT (checkCollatzERS (4 :: Int) ) ) Set.empty) (Just (4 :: Int))