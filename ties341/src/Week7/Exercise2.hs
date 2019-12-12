{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Week7.Exercise2 where

--import Week1.Exercise1
import Data.Foldable.Deriving (deriveFoldable)
import Data.Functor.Classes
import Data.Functor.Deriving (deriveFunctor)
import Data.Traversable.Deriving (deriveTraversable)
import Text.Show.Deriving (deriveShow1)

newtype Fix m = Fix {unFix :: m (Fix m)}

data BoolF a = BoolF {getBool :: Bool} deriving (Show, Eq, Read)
$(deriveShow1 ''BoolF)

-- getBool (unFix (x :: Bool')) 
-- Tuolla tavalla saadaan Bool ulos
-- Fix (BoolF True) :: Bool'
type Bool' = Fix BoolF



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


--newtype UusiBool = UB (Fix BoolF)



--newtype Bot = Bot Bot