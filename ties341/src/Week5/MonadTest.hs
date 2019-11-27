module MonadTest where
import Data.Monoid  
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State  
import Control.Monad.Except
import Data.Char

readtest :: Reader String (String,String)
readtest = do
    r <- ask
    rUpper <- asks (fmap toUpper)
    return (r,rUpper)  

logNumber :: Int -> Writer [String] Int  
logNumber x = writer (x, ["Got number: " ++ show x])  

multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    tell ["Gonna multiply these two"]
    return (a*b) 

gcd' :: Int -> Int -> Writer [String] Int  
gcd' a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        gcd' b (a `mod` b)  

testi1 :: Maybe String
testi1 = Just 3 >>= (\x -> 
        Just "!" >>= (\y -> 
        Just (show x ++ y)))
-- function monad is also reader monad!
testReader :: Reader Int String
testReader = reader (\x -> show x ++"!")

-- ask ottaa argumentin talteen!
readerDo :: Reader String Int
readerDo = do
    x <- ask
    y <- ask
    return (length x + length y) 


addStuff :: Int -> Int  
addStuff  = do  
    a <- (*2) 
    b <- (+10)  
    return (a+b) 

testi2 = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)

justH :: Maybe Char  
justH = do  
    (x:xs) <- Just ""  
    return x 

--listaMonad :: [Int]
listaMonad = do
    n <- [3,4,5]
    ch <- ['a','b']
    return (n,ch)

lista2 = do
    x <- [1..50]
    return x

calculateContentLen :: Reader String Int
calculateContentLen = do
    content <- ask
    return (length content);

-- Calls calculateContentLen after adding a prefix to the Reader content.
calculateModifiedContentLen :: Reader String Int
calculateModifiedContentLen = local ("Prefix " ++) calculateContentLen

mainCalc = do
    let s = "12345";
    let modifiedLen = runReader calculateModifiedContentLen s
    let len = runReader calculateContentLen s
    putStrLn $ "Modified 's' length: " ++ (show modifiedLen)
    putStrLn $ "Original 's' length: " ++ (show len)
type Stack = [Int]  

pop :: State Stack Int  
pop = state $ \(x:xs) -> (x,xs)  
  
push :: Int -> State Stack ()  
push a = state $ \xs -> ((),a:xs)

stackStuff :: State Stack Stack  
stackStuff = do  
    a <- pop 
    x <- get 
    if a == 5  
        then return x 
        else do
            push 3 
            push 8 
            return x
--get antaa tilan tuloksena 
-- put ottaa jonkun tilan ja pistää sen tilaksi