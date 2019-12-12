module Week7.Exercise1 where

import Data.Function (fix)

-- fix :: (a -> a) -> a
-- fix f = f (fix f)
-- f :: a -> a
-- (fix f) :: a
-- f (fix f) :: a
redefined_id :: a -> a
redefined_id x = fix (const x)

-- nonrecursive generators
-- fixed point (* 3) 0 = 0
-- f (fix f) = fix f
-- joten fix f on fixed point f:lle

redefined_append :: [a] -> [a] -> [a]
redefined_append   = fix (\f xs ys -> case xs of 
                            []     -> ys
                            (z:zs) -> z : (f zs ys))

-- onko (++) - käyttö tässä ok?
redefined_reverse :: [a] -> [a]
redefined_reverse = fix (\f xs -> case xs of
                            []     -> []
                            (y:ys) -> f ys ++ [y])


redefined_repeat :: a -> [a]
redefined_repeat = fix (\f x -> x : f x)

redefined_foldr :: (a -> b -> b) -> b -> [a] -> b
redefined_foldr = fix (\g f z xs -> case xs of
                                []     -> z
                                (y:ys) -> f y (g f z ys))


redefined_unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
redefined_unfoldr = fix (\g f b -> case f b of 
                            Just (x,y) -> x : g f y
                            Nothing -> [] )
  
redefined_fix :: (a -> a) -> a
redefined_fix = fix (\g f -> f (g f))
