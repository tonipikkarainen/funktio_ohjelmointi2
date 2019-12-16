{-# LANGUAGE ScopedTypeVariables #-}

module Week7.Exercise3 where

import Data.Char (intToDigit)
import Data.Foldable (find)
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Semigroup (Endo (..), stimesMonoid)
import Data.Set (Set (..))
import qualified Data.Set as Set
import Week4.Exercise3
import Week4.Exercise4
import Week3.Exercise3


closedStringBad :: String
closedStringBad = "let \
  \ two = 1 + 1 in let \
  \ three = 1 * (1 + two * 1) in let \
  \ five = let \
    \ four = two * two in \
    \ 1 + four in let \
  \ six = let \
    \ seven = 1 + six in \
    \ two * three in let \
  \ eight = two * four in let \
  \ nine = 0 + three * (three + 0) in \
  \ (0 * five + 1) + (three * (1 + nine) + eight * 0)"

-- muokattu 
closedDeepBad :: Expr
closedDeepBad = case (getParser expr) closedStringBad of
  Left e -> error (show e)
  Right (y,x) -> x
       
optimize :: Expr -> Expr
optimize = (appEndo . foldMap Endo) [elimDead,
  unifyAddZero,
  unifyMulOne,absorbZeroMul]

optimizePasses :: Int -> Expr -> Expr
optimizePasses n = (appEndo . stimesMonoid n . Endo) optimize

-- Omat funktiot:
-- True jos ei ole muuttujia tai viittauksia muuttujiin.

breadth :: Expr -> Int
breadth ex = case ex of
  Var x -> 0
  One  ->  0
  Zero ->  0
  Add x y -> 2 + (breadth x) + (breadth y)
  Mul x y -> 2 + (breadth x) + (breadth y)
  Let s x y -> 2 + (breadth x) + (breadth y)

isSimple :: Expr -> Bool
isSimple e = case e of
    Zero ->  True
    One  ->  True
    Let _ _ _ -> False
    Var _  -> False
    Add x y -> isSimple x && isSimple y
    Mul x y  -> isSimple x && isSimple y

unifyAddZero :: Expr -> Expr
unifyAddZero e = case e of
    Add Zero y -> unifyAddZero y
    Add x Zero -> unifyAddZero x
    Add x y -> Add (unifyAddZero x) (unifyAddZero y)
    Let s x y -> Let s (unifyAddZero x) (unifyAddZero y)
    Mul x y -> Mul (unifyAddZero x) (unifyAddZero y)
    x -> x

unifyMulOne :: Expr -> Expr
unifyMulOne e = case e of
    Mul One x -> unifyMulOne x
    Mul x One -> unifyMulOne x
    Mul x y -> Mul (unifyMulOne x) (unifyMulOne y)
    Let s x y -> Let s (unifyMulOne x) (unifyMulOne y)
    Add x y -> Add (unifyMulOne x) (unifyMulOne y)
    x -> x

absorbZeroMul :: Expr -> Expr
absorbZeroMul e = case e of
    Mul Zero x -> Zero
    Mul x Zero -> Zero
    Mul x y -> Mul (absorbZeroMul x) (absorbZeroMul y)
    Add x y -> Add (absorbZeroMul x) (absorbZeroMul y)
    Let s x y -> Let s (absorbZeroMul x) (absorbZeroMul y)
    x -> x 

-- Poistaa kaikki käyttämättömät bindit.
-- eli jos tulee vastaan Let - niin lähdetään katsomaan 
-- lauseketta, josta muuttuja pitäisi löytyä.
-- jos ei löydy niin vaihdetaan let:in tilalle 
-- lauseke (toinen ekspressio)
-- TODO: järkevämpi toteutus
-- Ensin substVar ?
elimDead :: Expr -> Expr
elimDead e = case e of
    Add x y -> Add (elimDead x) (elimDead y)
    Zero -> Zero
    Mul x y -> Mul (elimDead x) (elimDead y)
    One -> One
    Let s x y -> if sisaltaa y s then Let s (elimDead x) (elimDead y) else elimDead y
    Var x -> Var x
    where
        sisaltaa :: Expr -> String -> Bool
        sisaltaa e s = case e of
            Var x -> x == s 
            Zero -> False
            One -> False
            Add x y -> sisaltaa x s || sisaltaa y s
            Mul x y -> sisaltaa x s || sisaltaa y s
            Let u x y -> if u == s then False else (sisaltaa y s || sisaltaa x s)

-- The function breadth of type Expr -> Int should find the 
-- number of subexpressions in the expression. 
-- The resulting size should be unity for a constant.


-- | An improved version of the `showIntAtBase` function
-- from the `Numeric` module of the `base` package.
showIntAtBase' :: forall a. Integral a => a -> (Int -> Char) -> a -> ShowS
showIntAtBase' n f = let
  g :: a -> ShowS
  g p = case p `divMod` n of
    (0, 0) -> id
    (0, m) -> showChar (f (fromEnum m))
    (d, m) -> g d . showChar (f (fromEnum m)) in
  g

-- | A dual of the `keysSet` function
-- from the `Data.Map` module of the `containers` package.
elemsSet :: Ord a => Map k a -> Set a
elemsSet xs = Set.fromList (Map.elems xs)

-- | A copy of the `thenCmp` function
-- from the `Language.Haskell.TH.Syntax` module
-- of the `template-haskell` package.
thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ y = y
thenCmp x _ = x

namePrefixes :: [String]
namePrefixes = fmap pure ['x' .. 'z']

freshName :: Set String -> String
freshName css = case find (`Set.notMember` css)
  [(showString ds . showIntAtBase' 10 intToDigit n) mempty |
    n <- [0 :: Int ..], ds <- namePrefixes] of
  -- There are `on (*) toInteger maxBound (length namePrefixes)`
  -- possible names and they are generated in a sorted order
  -- to keep the behavior of the renamer predictable.
  Nothing -> error "Out of names"
  Just cs -> cs

findFree :: Expr -> Set String
findFree = let
  f :: Expr -> Set String
  f (Add x y) = f x <> f y
  f Zero = mempty
  f (Mul x y) = f x <> f y
  f One = mempty
  f (Let cs x y) = f x <> Set.delete cs (f y)
  f (Var cs) = Set.singleton cs in
  f

compareExpr :: Expr -> Expr -> Ordering
compareExpr = let
  f :: Map String String -> Map String String -> Expr -> Expr -> Ordering
  f css dss (Add x y) (Add z w) = f css dss x z `thenCmp` f css dss y w
  f _ _ Zero Zero = EQ
  f css dss (Mul x y) (Mul z w) = f css dss x z `thenCmp` f css dss y w
  f _ _ One One = EQ
  f css dss (Let cs x y) (Let ds z w) = f css dss x z `thenCmp` let
    es = freshName (findFree y <> findFree w) in
    f (Map.insert cs es css) (Map.insert ds es dss) y w
  f css dss (Var cs) (Var ds) = case (Map.lookup cs css, Map.lookup ds dss) of
    (Nothing, Nothing) -> cs `compare` ds
    -- We must not compare free variables to bound variables,
    -- because their ordering may be changed by the renamer.
    -- This is fine, because free variables are never renamed.
    (Nothing, Just _) -> LT
    (Just _, Nothing) -> GT
    (Just es, Just fs) -> es `compare` fs
  -- The remaining cases are chosen in such a way that
  -- "more constant" terms come before "more varying" ones.
  -- This kind of convention may seem arbitrary,
  -- but it comes up frequently in functional programming.
  f _ _ Add {} Zero {} = GT
  f _ _ Add {} Mul {} = LT
  f _ _ Add {} One {} = GT
  f _ _ Add {} Let {} = LT
  f _ _ Add {} Var {} = LT
  f _ _ Zero {} Add {} = LT
  f _ _ Zero {} Mul {} = LT
  f _ _ Zero {} One {} = LT
  f _ _ Zero {} Let {} = LT
  f _ _ Zero {} Var {} = LT
  f _ _ Mul {} Add {} = GT
  f _ _ Mul {} Zero {} = GT
  f _ _ Mul {} One {} = GT
  f _ _ Mul {} Let {} = LT
  f _ _ Mul {} Var {} = LT
  f _ _ One {} Add {} = LT
  f _ _ One {} Zero {} = GT
  f _ _ One {} Mul {} = LT
  f _ _ One {} Let {} = LT
  f _ _ One {} Var {} = LT
  f _ _ Let {} Add {} = GT
  f _ _ Let {} Zero {} = GT
  f _ _ Let {} Mul {} = GT
  f _ _ Let {} One {} = GT
  f _ _ Let {} Var {} = LT
  f _ _ Var {} Add {} = GT
  f _ _ Var {} Zero {} = GT
  f _ _ Var {} Mul {} = GT
  f _ _ Var {} One {} = GT
  f _ _ Var {} Let {} = GT in
  f mempty mempty

instance Eq Expr where
  x == y = compareExpr x y == EQ

instance Ord Expr where
  compare = compareExpr