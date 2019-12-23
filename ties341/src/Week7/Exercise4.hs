{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Week7.Exercise4 where

import Control.Arrow
import Data.Function (on)
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Semigroup (Endo (..), stimesMonoid)
import Data.Set (Set (..))
import qualified Data.Set as Set
import Week4.Exercise3
import Week7.Exercise2
import Week7.Exercise3

type Algebra m a = m a -> a

-- | Algebraic version of the `Endo` type
-- from the `Data.Monoid` module of the `base` package.
newtype Embed m = Embed {appEmbed :: Algebra m (Fix m)}

instance Semigroup (Embed m) where
  Embed g <> Embed f = Embed (g . unFix . f)

instance Monoid (Embed m) where
  mempty = Embed Fix

testFixExp = Fix (AddF (Fix (MulF (Fix OneF) (Fix ZeroF))) (Fix ZeroF))

optimizeAlg' :: Algebra ExprF Expr'
optimizeAlg' = (appEmbed . foldMap Embed) [unifyAddZero',
    unifyMulOne', assocAdd', assocMul']

optimize' :: Expr' -> Expr'
optimize' = cata optimizeAlg'

isSimple' :: Algebra ExprF Bool 
isSimple' e = case e of
    ZeroF       ->  True
    OneF        ->  True
    LetF _ _ _  ->  False
    VarF _      ->  False
    AddF x y    ->  x && y
    MulF x y    ->  x && y


unifyAddZero' ::  Algebra ExprF Expr' 
unifyAddZero' e = case e of
    AddF (Fix ZeroF) y  -> y
    AddF x (Fix ZeroF)  -> x
    x                   -> Fix x

unifyMulOne' :: Algebra ExprF Expr' 
unifyMulOne' e = case e of
    MulF (Fix OneF) x -> x
    MulF x (Fix OneF) -> x
    x                 -> Fix x

absorbZeroMul' :: Algebra ExprF Expr' 
absorbZeroMul' e = case e of
    MulF (Fix ZeroF) x -> Fix ZeroF
    MulF x (Fix ZeroF) -> Fix ZeroF
    x                  -> Fix x 

assocAdd' :: Algebra ExprF Expr' 
assocAdd' e = case e of
  AddF (Fix (AddF x z)) y -> Fix (AddF x (Fix (AddF z y)) )
  x -> Fix x


assocMul' :: Algebra ExprF Expr' 
assocMul' e = case e of
  MulF (Fix (MulF x z)) y -> Fix (MulF x (Fix (MulF z y)) )
  x -> Fix x

