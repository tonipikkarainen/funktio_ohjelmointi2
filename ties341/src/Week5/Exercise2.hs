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

checkCollatzSRE :: (Intlike a, Intlike b) =>
  a -> ExceptT Problem (ReaderT (Maybe Int) (State (Set a))) b
checkCollatzSRE = let
  f :: (Intlike a, Intlike b) =>
    a -> ExceptT Problem (ReaderT (Maybe Int) (State (Set a))) b
  f n = do
    ps <- (lift . lift) get
    if Set.member n ps then
      throwE Loop else do
      ms <- lift ask
      case ms of
        Nothing -> if Set.size ps >= maxBound then
          throwE (Bound Cache) else
          (lift . lift) (put (Set.insert n ps))
        Just m -> if Set.size ps >= m then
          pure () else
          (lift . lift) (put (Set.insert n ps))
      if abs n == 1 then
        pure 0 else do
        if not (safeToCollatz n) then
          throwE (Bound Value) else let
          p = collatz n in do
          q <- f p
          if not (safeToCount q) then
            throwE (Bound Counter) else
            pure (count q) in
  \ n -> catchE (f n) $ \ e -> case e of
    Bound Cache -> mapExceptT (local (const collatzBound)) (f n)
    _ -> throwE e

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