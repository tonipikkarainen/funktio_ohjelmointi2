{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Week7.Exercise2 where

import Data.Foldable.Deriving (deriveFoldable)
import Data.Functor.Classes
import Data.Functor.Deriving (deriveFunctor)
import Data.Traversable.Deriving (deriveTraversable)
import Text.Show.Deriving (deriveShow1)
import Data.Void
import Data.Stream.Infinite



newtype Fix m = Fix {unFix :: m (Fix m)}

-- Bool
data BoolF a = TrueF | FalseF deriving (Show, Eq, Read)
$(deriveShow1 ''BoolF)

type Bool'   = Fix BoolF

toBool :: Bool' -> Bool
toBool x = case x of 
  Fix TrueF -> True
  Fix FalseF -> False

fromBool :: Bool -> Bool'
fromBool x = case x of
  True -> Fix TrueF
  False -> Fix FalseF

-- (1) Päästään kulkemaan edestakaisin menettämättä informaatiota.
-- Bool' ja Bool ovat siis isomorfiset.


-- Maybe
data MaybeF a r = Juuri a | Ei
$(deriveShow1 ''MaybeF)

type Maybe' a   = Fix (MaybeF a)

-- Samanlainen isomorfismi kuin kohdassa 1
-- voidaan helposti kirjoittaa myös 
-- Maybe' :n ja Maybe:n välille.


-- Either
data EitherF a b r = Vasen a | Oikea b
$(deriveShow1 ''EitherF)

type Either' a b   = Fix (EitherF a b)

-- Samanlainen isomorfismi kuin kohdassa 1
-- voidaan helposti kirjoittaa myös 
-- Either' :n ja Either:n välille.

-- ()

data UnitF a = UnitF
$(deriveShow1 ''UnitF)

type Unit'   = Fix (UnitF)

-- Samanlainen isomorfismi kuin kohdassa 1
-- voidaan helposti kirjoittaa myös 
-- Unit' :n ja ():n välille.


-- [] a

data ListF a r = NilF | ConsF a r deriving (Show, Eq, Read)
$(deriveFoldable ''ListF)
$(deriveFunctor ''ListF)
$(deriveShow1 ''ListF)
$(deriveTraversable ''ListF)

type List' a   = Fix (ListF a)

toList :: List' a -> [a]
toList xs = case xs of
  Fix NilF -> []
  Fix (ConsF x y) -> x : toList y 

fromList ::  [a] -> List' a 
fromList xs = case xs of
  [] -> Fix NilF
  y:ys -> Fix (ConsF y (fromList ys))

-- (5) Päästään kulkemaan edestakaisin menettämättä informaatiota.
-- List' ja [] ovat siis isomorfiset.

-- Testiarvo
val :: List' Int
val = Fix (ConsF 1 (Fix (ConsF 20 (Fix NilF))))

-- Voidaan laskea myös List':n pituus:
lenAlg :: ListF e Int -> Int
lenAlg (ConsF e n) = n + 1
lenAlg NilF = 0

-- Tällä voidaan testata 
-- erilaisia algebroja.
-- Esim. cata lenAlg val == 2
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix


-- Void
data VoidF r 
type Void'   = Fix VoidF

-- Void ei sisällä informaatiota eikä myöskään Void'.



-- Identity a
data IdentityF a r = IdentityF {runIdentityF :: a}
$(deriveShow1 ''IdentityF)
$(deriveFunctor ''IdentityF)


type Identity' a   = Fix (IdentityF a) 

-- Tämäkin saataisiin helposti muunnettua Identityksi.

-- data Stream a = a :< Stream a
-- Stream

data StreamF a r = a :|> r 
type Stream' a   = Fix (StreamF a)

toStream :: Stream' a -> Stream a 
toStream (Fix (x :|> y)) = x :> toStream y

fromStream :: Stream a -> Stream' a
fromStream (x :> y) = Fix (x :|> (fromStream y)) 

-- (8) Päästään kulkemaan edestakaisin menettämättä informaatiota.
-- Stream' ja Stream ovat siis isomorfiset.

-- Tree

data TreeF a r = NodeF {rootLabel :: a, subForest :: [r] }
type Tree' a   = Fix (TreeF a)

-- Fix : n avulla muodostettu tyyppi tehty samoin kuin
-- muissa kohdissa.

-- Expr
data ExprF r = AddF r r   
             | ZeroF            
             | MulF r r         
             | OneF              
             | LetF String r r  
             | VarF String  

$(deriveFoldable ''ExprF)
$(deriveFunctor ''ExprF)
$(deriveShow1 ''ExprF)
$(deriveTraversable ''ExprF)

type Expr' = Fix ExprF

pattern Add' :: Expr' -> Expr' -> Expr'
pattern Add' x y = Fix (AddF x y)

pattern Mul' :: Expr' -> Expr' -> Expr'
pattern Mul' x y = Fix (MulF x y)


-- Nyt esim. :
-- Add' (Mul' (Fix OneF) (Fix ZeroF)) (Fix OneF) 
-- tuottaa oikean näköisen rakenteen:
-- Fix (AddF (Fix (MulF (Fix OneF) (Fix ZeroF))) (Fix OneF))


-- Free
data FreeF f a r = PureF a | FreeF (f r) 
type Free' f a   = Fix (FreeF f a)

-- Fix : n avulla muodostettu tyyppi tehty samoin kuin
-- muissa kohdissa.

-- Cofree
data CofreeF f a r =  a :< (f r)
type  Cofree' f a  = Fix (CofreeF f a)

-- Fix : n avulla muodostettu tyyppi tehty samoin kuin
-- muissa kohdissa.

-- Fix m
data FixF m r = FixF {unFixF :: m r}
type Fix' m   = Fix (FixF m)

-- Fix : n avulla muodostettu tyyppi tehty samoin kuin
-- muissa kohdissa.


-- | We can derive a `Show` instance for `Fix` by
--
-- * adding the `deriving Show` clause to the `newtype` declaration,
-- * observing the error messages,
-- * replacing the `deriving Show` clause with the inferred
--   `deriving instance Show (m (Fix m)) => Show (Fix m)` declaration,
-- * observing the error messages,
-- * turning on the `StandaloneDeriving` extension,
-- * observing the error messages,
-- * turning on the `UndecidableInstances` extension,
-- * observing the worrying lack of error messages,
-- * turning on the `-ddump-deriv` option,
-- * observing the dump messages,
-- * replacing the `deriving instance Show (m (Fix m)) => Show (Fix m)`
--   declaration with the dumped instance,
-- * turning off the `StandaloneDeriving` and
--   `UndecidableInstances` extensions,
-- * changing the `Show (m (Fix m))` constraint to the `Show1 m` constraint,
-- * replacing the `showsPrec` call with the `showsPrec1` call,
-- * observing the error messages,
-- * importing the `Data.Functor.Classes` module,
-- * observing the reassuring lack of error messages,
-- * observing the excessively verbose mess that the instance produces,
-- * writing less verbose variations that break the `Show` laws,
-- * using splices to make it possible to choose one of the variations,
-- * observing the error messages,
-- * turning on the `TemplateHaskell` extension,
-- * observing the error messages,
-- * replacing the splices with preprocessing directives,
-- * turning off the `TemplateHaskell` extension,
-- * observing the error messages,
-- * turning on the `CPP` extension,
-- * observing the eventual lack of error messages and
-- * sacrificing a child to Cheibriados.
--
-- We can then obtain a `Show` instance for `Fix F`
-- (or any partial application thereof) by
--
-- * turning on the `TemplateHaskell` extension,
-- * splicing `deriveShow1 ''F` to derive a `Show1` instance for `F` and
-- * letting the compiler figure out the rest.
--
-- You may choose your preferred variation of `Show` based on your alignment.
#define NEUTRAL_GOOD

#ifdef LAWFUL_GOOD
instance Show1 m => Show (Fix m) where
  showsPrec n (Fix x) = showParen (n >= 11) (showString "Fix {" .
    showString "unFix = " . showsPrec1 0 x . showString "}")
#else
#ifdef NEUTRAL_GOOD
instance Show1 m => Show (Fix m) where
  showsPrec n (Fix x) = showParen (n >= 11)
    (showString "Fix " . showsPrec1 11 x)
#else
#ifdef CHAOTIC_GOOD
instance Show1 m => Show (Fix m) where
  showsPrec n (Fix x) = showsPrec1 n x
#else
#error Alignment
#endif
#endif
#endif
