module Week8.Exercise1 where

import Data.Stream.Infinite (Stream (..))
import qualified Data.Tree as Rose (Forest (..), Tree (..))
--import qualified Data.Tree.Binary.Inorder as Binary (Tree (..))
newtype Fix m = Fix {unFix :: m (Fix m)}

{-
Derivative of Maybe a.
Derivative of Join (,) a.
Derivative of [] a.
Derivative of Stream a from the streams package.
Derivative of Tree a from the binary-tree package.
Derivative of Tree a from the containers package.
-}

-- Maybe

{-
 data Maybe a = Just a | Nothing
 Sum - type
   ~  Either a () = a | ()
   voidaan ilmaista muodossa ~ a + 1
   dlt_a (a + 1) 
 = dlt_a a + dlt_a 1 
 = 1 + 0 
 = 1 

 -> eli ()
-}

data DMaybe a = DMaybe

-- Join 

{-
Join (,) a
newtype Join p a = Join {runJoin :: p a a}	 

Tämä sisältää parin (a,a).

vastaa muotoa - a x a

  dlt_a (a x a) 
= (dlt_a a) x a + a x (dlt_a a)
= 1 x a + a x 1
= a + a

-}

type DJoin a = Either a a

-- Lista

{-
[] a 

ListF a = NilF | ConsF a r
List a = Fix (ListF a)

ListF a  ~ 1 + a x r

Fix ~ u_

  dlt_a ([a]) 
= dlt_a (u_r (1 + a x r))
= u_s ((dlt_a (1 + a x r) [r <- u_r (1 + a x r)]) + 
  dlt_r (1 + a x r)[r <- u_r (1 + a x r)] x s )
= u_s (r [r <- u_r (1 + a x r)]  +  a [r <- u_r (1 + a x r)] x s )
= u_s (u_r (1 + a x r) + a x s)
= u_s (c  + a x s)  -- c on lista
= c x u_s (1  + a x s)  
= c x c 

-}

type DList a = ([a], [a])


--- Stream 

data StreamF a r = ConsStreamF a r 
type Stream' a   = Fix (StreamF a)

-- Stream' a vastaa muotoa:
-- u_r ( a x r )
{-
  dlt_a (Stream' a )
= dlt_a (u_r (a x r )) <- merkitään sisältöä S:llä
= u_z (dlt_a (a x r) [r <- S] + dlt_r ( a x r ) [r <- S] x z)
... ratkaisin paperilla
= S x u_s ( 1 + a x s )

-}
type StreamD a = (Stream a , [a]) 

-- Tree

data Tree a = Leaf | Node (Tree a) a (Tree a)

data TreeF a r = LeafF | NodeF r a r

type Tree' a = Fix (TreeF a)

-- Tree
{-
  TreeF -- 1 + r x a x r

  dlt_a (Tree' a) 
= dlt_a (u_r (1 + r x a x r)) <- merkitään sisältöä T:llä
= u_s ( dlt_a (1 + r x a x r) [r <- T] dlt_r (1 + r x a x r) [r <- T] x S )

    ... (ratkaisin paperilla)
= (T x T) x (u_s ( 1 + 2 x ( a x T ) x s))
-}

type DTree a = ((Tree a, Tree a) , [(Bool, a, Tree a)]) 

-- Tree (from containers)
{-
data Rose.Tree a
  = Rose.Node {Rose.rootLabel :: a,
     Rose.subForest :: Rose.Forest a}
  -}

data RoseTreeF a r = RoseNodeF {rootLabel :: a, subForest :: [r] }
type RoseTree' a = Fix (RoseTreeF a)

type DRoseTree a = ([Rose.Tree a] , [(a , ([Rose.Tree a],[Rose.Tree a]))])


{-
  RoseTree' vastaa muotoa 
  a * u_r ( 1 x (u_s (1 + r x s ) ) ) 
= 
... paperilla ratkaistu

=

 u_y ( [R]  + (a * [R] * [R] * y) 

=  [ R ] * [ a * [R] * [R] ]

-}

