{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Week7.Exercise2 where

import Data.Foldable.Deriving (deriveFoldable)
import Data.Functor.Classes
import Data.Functor.Deriving (deriveFunctor)
import Data.Traversable.Deriving (deriveTraversable)
import Text.Show.Deriving (deriveShow1)
import Data.Void

newtype Fix m = Fix {unFix :: m (Fix m)}
-- (getBool . unFix) (x :: Bool')) 
--
-- Tuolla tavalla saadaan Bool ulos
-- Fix (BoolF True) :: Bool'
-- tässä tullaan tekemään se, että
-- BoolF:n argumentti a korvataan
-- Fix BoolF:llä
-- Jolloin BoolF (Fix BoolF) = BoolF
-- parametri ei vaikuta tulokseen.

-- Bool
data BoolF a = BoolIn {getBool :: Bool} deriving (Show, Eq, Read)
$(deriveShow1 ''BoolF)

type Bool'   = Fix BoolF

-- Maybe
data MaybeF a r = MaybeIn {getMaybe :: Maybe a}
$(deriveShow1 ''MaybeF)

type Maybe' a   = Fix (MaybeF a)


-- Either
data EitherF a b r = EitherIn {getEither :: Either a b}
$(deriveShow1 ''EitherF)

type Either' a b   = Fix (EitherF a b)

-- ()

data UnitF a = UnitIn {getUnit :: ()}
$(deriveShow1 ''UnitF)

type Unit'   = Fix (UnitF)

-- [] a

data ListF a r = NilF | ConsF a r deriving (Show, Eq, Read)
$(deriveFoldable ''ListF)
$(deriveFunctor ''ListF)
$(deriveShow1 ''ListF)
$(deriveTraversable ''ListF)

type List' a   = Fix (ListF a)


val :: List' Int
val = Fix (ConsF 1 (Fix (ConsF 20 (Fix NilF))))

lenAlg :: ListF e Int -> Int
lenAlg (ConsF e n) = n + 1
lenAlg NilF = 0

-- Tällä voidaan testata 
-- erilaisia algebroja.
-- Esim. cata lenAlg val == 2
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

-- Void
data VoidF r = VoidF {getVoid :: Void }
type Void'   = Fix VoidF

-- Identity a
data IdentityF a r = IdentityF {runIdentity :: a}
$(deriveShow1 ''IdentityF)
$(deriveFunctor ''IdentityF)


type Identity' a   = Fix (IdentityF a) 

-- data Stream a = a :< Stream a
-- Stream

data StreamF a r = a :> r 
type Stream' a   = Fix (StreamF a)

-- Tree
-- type Forest a = [Tree a]
-- data Tree a = Node	{rootLabel :: a,	subForest :: Forest a}

data TreeF a r = NodeF {rootLabel :: a, subForest :: [r] }
type Tree' a   = Fix (TreeF a)


-- Expr
data ExprF r = Add r r   
             | Zero            
             | Mul r r         
             | One              
             | Let String r r  
             | Var String  

$(deriveFoldable ''ExprF)
$(deriveFunctor ''ExprF)
$(deriveShow1 ''ExprF)
$(deriveTraversable ''ExprF)

type Expr' = Fix ExprF

-- Free
data FreeF f a r = PureF a | FreeF (f r) 
type Free' f a   = Fix (FreeF f a)

-- Cofree
data CofreeF f a r =  a :< (f r)
type  Cofree' f a  = Fix (CofreeF f a)


-- Fix m
data FixF m r = FixF {unFixF :: m r}
type Fix' m   = Fix (FixF m)



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
