module Week4.Exercise4 where

import Control.Applicative
import Week3.Exercise3
import Week4.Exercise3

closedString :: String
closedString = "let \
  \ two = 1 + 1 in let \
  \ three = 1 + two in let \
  \ nine = three * three in \
  \ 1 + three * (1 + nine)"

openString :: String
openString = "1 + undefined"


expr :: Parser Expr
expr = do 
    x <- add
    eof
    return x

zero :: Parser Expr
zero = do 
    x <- (single '0')
    return Zero

one :: Parser Expr
one = do 
    x <- (single '1')
    return One

add :: Parser Expr
add = do
    x <- addAndMul <|> mul
    return x

mul :: Parser Expr
mul = do
    x <-  mulAndOther <|> other
    return x

addAndMul :: Parser Expr    
addAndMul = do
    x <- (mul <* single '+')
    y <- add
    return (Add x y)

mulAndOther :: Parser Expr
mulAndOther = do
    x <- (other <* single '*') 
    y <- mul
    return (Mul x y)

-- muokkaa vielä
other :: Parser Expr
other = do
    x <- (zero <|> one)
    return x

{-    
do
 x <- (zero <|> one)
 single '+'
 y <- (zero <|> one)
 eof
 return (Add x y) 
-}
    


