{-# LANGUAGE EmptyCase #-}

module Week6.Exercise3 where

import Data.Void

-- (~) ekvivalenssirelaatio on isomorfismi tyyppien välillä
-- (+) summatyyppi on Either
-- (0) on Void
-- (1) on ()
-- (*) on (,)
-- exponentiation on ( -> )

-- Tässä käytetään Either ja tuple
-- Onko niin, että voitaisiin summatyyppinä 
-- käyttää mitä tahansa summatyyppiä:
-- vaikka data a b c = First a | Second b | Third c
-- ja samat lait pätisivät?
-- Silloin tuo tietty ilmentäisi kolmen 
-- alkion summaa?

-- equivalence is reflexive
f_eqrefl :: (a -> a)
f_eqrefl = id

g_eqrefl :: (a -> a)
g_eqrefl = id

-- equivalence is symmetric
f_eqsymm :: (a -> b, b -> a) -> (b -> a, a -> b )
f_eqsymm (x,y) = (y,x)
-- Tämä on swap-funktio

-- equivalence is transitive
f_eqtrans ::  (a -> b, b -> a) -> (b -> c, c -> b) -> (a -> c, c -> a)
f_eqtrans (f,g) (k,j) = (k.f , g.j)

-- addition is proper
f_addprop :: (a -> b, b -> a) -> (c -> d, d -> c) -> (Either a c  -> Either b d, Either b d -> Either a c)
f_addprop (f,g) (k,j) =  (\x -> case x of 
    Left y -> Left (f y)
    Right y -> Right (k y), \x -> case x of
        Left y -> Left (g y)
        Right y -> Right (j y))

-- addition is associative
f_addassoc :: Either a ( Either b c ) -> Either ( Either a b ) c
f_addassoc x = case x of
    Left y -> Left (Left y)
    Right (Left y) -> Left (Right y)
    Right (Right y) -> Right y

g_addassoc :: Either ( Either a b ) c -> Either a ( Either b c )   
g_addassoc x = case x of
    Left (Left y)  -> Left y
    Left (Right y) -> Right (Left y)
    Right y        -> Right (Right y)

-- addition is commutative
f_addcomm :: Either a b -> Either b a
f_addcomm x = case x of
    Left y -> Right y
    Right y -> Left y

-- Samaa toimii molempiin suuntiin.
-- tämä on swapEither-funktio Data.Either.Combinators - moduulissa

-- zero is unitary with respect to addition on the left
f_addzleft ::  Either Void a -> a
f_addzleft x = case x of
    Right y -> y
    Left l  -> absurd l

g_addzleft :: a -> Either Void a
g_addzleft x = Right x

-- zero is unitary with respect to addition on the right
-- x + 0 ~ x
-- Toimii samalla tavalla kuin edellinen
-- mutta Void on Right - paikalla

-- multiplication is proper
f_mulprop :: (a -> b, b -> a) -> (c -> d, d -> c) -> ((a,c) -> (b,d), (b,d) -> (a,c))
f_mulprop (f,g) (k,j) = (\(x,y) -> (f x, k y), \(x,y) ->  (g x , j y)) 



-- multiplication is associative
f_mulassoc :: (a , (b,c)) -> ((a,b), c )
f_mulassoc (x , (y,z)) = ((x,y),z) 

g_mulassoc :: ((a,b),c) -> (a , (b,c))
g_mulassoc ((x,y),z) = (x , (y,z))

-- multiplication is commutative

f_mulcom :: (a,b) -> (b,a)
f_mulcom (x,y) = (y,x)

-- sama funktio toimii myös toiseen suuntaan.
-- Data.Tuple - moduulista löytyy kyseinen funktio nimellä swap.

-- one is unitary with respect to multiplication on the left
f_unitmulL :: ((),a) -> a
f_unitmulL (x,y) = y 

g_unitmulL :: a -> ((),a)
g_unitmulL x = ((),x)

-- one is unitary with respect to multiplication on the right
-- Right menee samalla tavoin, mutta unit-tyyppi on 
-- oikealla parissa.

-- x * (y + z) ~ x * y + x * z

-- multiplication is distributive over addition on the left
f_distaddL :: (a, Either b c) -> Either (a,b) (a,c)
f_distaddL (x, y) = case y of 
    Left k -> Left (x,k)
    Right j -> Right (x,j)

g_distaddL :: Either (a,b) (a,c) -> (a, Either b c)
g_distaddL x = case x of
    Left (z,w) -> (z, Left w)
    Right (k,j) ->  (k, Right j)

-- multiplication is distributive over addition on the right
-- Oikealta distributiivinen menee samalla idealla kuin äskeinen.

-- zero is absorbing with respect to multiplication on the left
f_mulzeroL :: (Void,a) -> Void
f_mulzeroL (x,y) = x
-- sama kuin fst

g_mulzeroL :: Void -> (Void,a)
g_mulzeroL x = (x, absurd x)

-- zero is absorbing with respect to multiplication on the right
-- Oikealta nollalla kertominen menee samalla idealla.

--  exponentiation is proper
f_expprop :: (a -> b, b -> a) -> (c -> d, d -> c) -> ((a -> c) -> (b -> d) ,(b -> d) -> (a -> c)  )
f_expprop (f,g) (k,j) = (\ac -> k . ac . g , \bd -> j . bd . f)

-- exponentiation is distributive over addition and multiplication on the right
f_expdistadd :: (Either b c -> a) -> (b -> a, c -> a )
f_expdistadd f = (\x -> f (Left x), \y -> f (Right y))  

g_expdistadd ::  (b -> a, c -> a ) ->  (Either b c -> a)
g_expdistadd (f, g) = \y -> case y of 
                            Left x -> f x
                            Right x -> g x 

-- exponentiation is associative over multiplication on the right
-- Tämä oli esimerkkinä.


-- exponentiation is distributive over multiplication on the left

f_distmul :: (c -> (a,b)) -> (c -> a , c -> b)
f_distmul f = ( fst . f , snd . f )

g_distmul :: (c -> a , c -> b) -> (c -> (a,b)) 
g_distmul ( f , g ) = \x -> (f x, g x)

-- one is a unit of exponentiation on the left
f_oneunitexp :: (() -> a) -> a 
f_oneunitexp f = f ()

g_oneunitexp :: a -> (() -> a)
g_oneunitexp x = \() -> x

-- one is absorbing with respect to exponentiation on the right
f_oneabsorb :: (a -> ()) -> ()
f_oneabsorb f = ()

g_oneabsorb :: () -> (a -> ())
g_oneabsorb y = \x -> ()

-- zero is absorbing with respect to exponentiation on the left
f_zeroabsorb :: ( Void -> a ) -> ()
f_zeroabsorb f = ()

g_zeroabsorb :: () -> ( Void -> a )
g_zeroabsorb () = absurd

