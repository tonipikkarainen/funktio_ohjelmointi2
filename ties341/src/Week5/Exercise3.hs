{-# LANGUAGE FlexibleContexts #-}

module Week5.Exercise3 where

import Data.Int (Int8 (..), Int16 (..))
import Data.Set (Set (..))
import qualified Data.Set as Set
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
--import Control.Monad.MonadStack 
import Week5.Exercise2

{-
Take the programs from exercise 2 and translate 
them into a single program that uses monad transformers 
with functional dependencies. That is, find the appropriate 
type classes for the monads, give the programs a common 
type that does not fix the stacking order, remove the 
explicit lifts and run the resulting program with the old 
helper function.
-}

foo :: (Intlike a, Intlike b, MonadReader (Maybe Int) m, MonadError Problem m, MonadState (Set a) m) => a -> m b  
foo =  let
    f :: (Intlike a, Intlike b, MonadReader (Maybe Int) m, MonadError Problem m, MonadState (Set a) m) => a -> m b 
    f n = do
      ps <- get                         
      if Set.member n ps then                         
        throwError Loop else do                           
        ms <- ask                                
        case ms of                                    
          Nothing -> if Set.size ps >= maxBound then  
            throwError (Bound Cache) else                
            (put (Set.insert n ps))     
          Just m -> if Set.size ps >= m then          
            pure () else  
            (put (Set.insert n ps))     
        if abs n == 1 then                            
          pure 0 else do                              
          if not (safeToCollatz n) then               
            throwError (Bound Value) else let             
            p = collatz n in do                       
            q <- f p                                  
            if not (safeToCount q) then               
              throwError (Bound Counter) else            
              pure (count q) in                       
    \ n -> f n
-- miten saadaan virheen tarkistus mukaan??

        --catchError (f n) $ \ e -> case e of
      --Bound Cache ->  mapExcept (local (const collatzBound)) (f n)
      --_ ->  throwError e
runCheckCollatz2 x = runState ((runReaderT (runExceptT (g x)) ) (Just maxBound)) Set.empty where
  g :: Int8 -> ExceptT Problem (ReaderT (Maybe Int) (State (Set Int8))) Int
  g =  foo  
