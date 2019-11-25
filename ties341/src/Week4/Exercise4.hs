module Week4.Exercise4 where
import Control.Applicative
import Week3.Exercise3
import Week4.Exercise3

-- Ekspressioparseri 
-- Tekijä: Toni Pikkarainen 
-- pvm: 22.11.2019

-- Parseri, jolla voidaan parsia
-- erään semiringin ekspessiot.
-- Ekspressiot voidaan evaluoida
-- Week4.Exercise3 :sta löytyvällä
-- deepEval funktiolla.
--

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

-- Nolla
zero :: Parser Expr
zero = do 
    x <- (single '0')
    return Zero

-- Ykkönen
one :: Parser Expr
one = do 
    x <- (single '1')
    return One

-- Lisäys tai kertolasku   
add :: Parser Expr
add = do
    x <- addAndMul <|> mul
    return x

-- Kertolasku tai muu
mul :: Parser Expr
mul = do
    x <-  mulAndOther <|> other
    return x

--  Kaksi ekspressiota ja "plus" välissä
addAndMul :: Parser Expr    
addAndMul = do
    x <- mul 
    single '+'
    y <- add
    return (Add x y)

-- Kaksi ekspressiota ja "kertomerkki" välissä
mulAndOther :: Parser Expr
mulAndOther = do
    x <- other 
    single '*'
    y <- mul
    return (Mul x y)

-- Nolla, ykkönen, sulut, let-lause tai muuttuja
other :: Parser Expr
other = do
    spaces
    x <- (zero <|> one <|> sub <|> let' <|> var)
    spaces
    return x

-- Jotain sulkujen sisällä
sub :: Parser Expr
sub = do
    single '('
    x <- add 
    single ')'
    return x

-- Let-lause  
let' :: Parser Expr
let' = do 
    chunk "let"
    spaces
    x <- ident
    spaces
    single '='
    spaces
    y <- add
    spaces
    chunk "in"
    spaces
    z <- add
    spaces
    return (Let x y z)

-- Muuttuja
var :: Parser Expr 
var = do 
    spaces
    x <- ident 
    spaces
    return (Var x)   


-- Muuttujan nimi.
-- Pitää alkaa pienellä kirjaimella.
-- Saa sisältää isoja/pieniä kirjaimia, numeroita ja hipsuja.
-- Ei saa olla sana "let" tai "in"
ident :: Parser String
ident = do 
    x <- small
    y <- many opt_chars
    if ((x:y) == "let" || (x:y) == "in") 
        then 
            do
            fail_oma -- empty
            return (x:y)
        else
            return (x:y)

-- Valittavat merkit muuttujan nimeen.
opt_chars :: Parser Char
opt_chars = do
    x <- small <|> large <|> digit <|> prime
    return x

-- Merkkikokoelmat
small = oneOf ['a'..'z'] <|> single '_'
large = oneOf ['A'..'Z']
digit = oneOf ['1'..'9']
prime = single '\''
spaces = many (oneOf ['\t','\n','\v' ,'\f','\r',' ']) 

-- Tällä voidaan suoraan laskea jonkun merkkijonon
-- arvo.
-- Esim. 
-- # evaluate closedString 
-- # Just 31
evaluate :: Num a => String -> Maybe a
evaluate s = case (getParser expr) s of
    Right (x,y) -> evalDeep' y mempty
    Left _ -> Nothing

-- Kielioppi, jonka mukaan parsitaan:    
{-
grammar ExprLR ;

expr : add ;
add : add '+' mul | mul ;
mul : mul '*' other | other ;
other : sub | zero | one | let | var ;
sub : '(' add ')' ;
zero : '0' ;
one : '1' ;
let : 'let' Ident '=' add 'in' add ;
var : Ident ;

Ident : Small ( Small | Large | Digit | Prime ) *
  { ! getText().equals("let") && ! getText().equals("in") }? ;
Small : [a-z] | [_] ;
Large : [A-Z] ;
Digit : [0-9] ;
Prime : ['] ;
Space : [\t\n\u000b\f\r ] + -> skip ;
-}