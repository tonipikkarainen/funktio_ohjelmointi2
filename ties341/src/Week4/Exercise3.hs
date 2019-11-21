module Week4.Exercise3 where

import Control.Exception
import Data.Map (Map (..))
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
-- Given a shallowly-embedded closed term representing 31

-- a shallowly-embedded open term
openShallow :: Num a => a
openShallow = 1 + undefined
--evaluator for shallowly-embedded terms

evalShallow :: Num a => a -> Maybe a
evalShallow x = unsafePerformIO (catch (seq x (pure (Just x)))
  (\ e -> seq (e :: SomeException) (pure Nothing)))

-- unsafePerformIO ::  IO a -> a
-- catch :: IO a -> (e -> IO a) -> IO a
-- seq :: a -> b -> b  -- x (pure (Just x)) -> eli nostaa x:n
-- IO (Maybe a) :ksi
-- catchin eka: (IO (Just x)) toka: 
-- (\ e -> seq (e :: SomeException) (pure Nothing))


--implement the corresponding constructions for deeply-embedded terms  
data Expr = Add Expr Expr | Zero | Mul Expr Expr | One |
  Let String Expr Expr | Var String
  deriving Show

openDeep :: Expr
openDeep = Add One undefined

closedShallow :: Num a => a
closedShallow = let
  two :: Num a => a
  two = 1 + 1 in let
  three :: Num a => a
  three = 1 + two in let
  nine :: Num a => a
  nine = three * three in
  1 + three * (1 + nine)

closedDeep :: Expr
closedDeep = 
    Let "two" (Add One One) 
    (Let "three" (Add One (Var "two"))
    (Let "nine" (Mul (Var "three") (Var "three")) 
    (Add One (Mul (Var "three") (Add One (Var "nine"))))))



evalDeep :: Num a => Expr -> Maybe a
evalDeep x = unsafePerformIO (catch (seq (evalApuDeep x) (pure (Just (evalApuDeep x))))
    (\ e -> seq (e :: SomeException) (pure Nothing)))

-- Apufunktio ekspressioiden evaluoimiseksi    
evalApuDeep :: Num a => Expr -> a
evalApuDeep Zero = 0
evalApuDeep One =  1
evalApuDeep (Add y x) = (evalApuDeep y) + (evalApuDeep x)
evalApuDeep (Mul y x) = (evalApuDeep y) * (evalApuDeep x)
evalApuDeep (Let muuttuja m_sisalto lauseke) = evalApuDeep (sijoita muuttuja m_sisalto lauseke)
    

-- Tämän avulla sijoitetaan muuttujien paikalle
-- muuttujia vastaavat ekspressiot 
-- Let - konstruktorin tapauksessa.
sijoita :: String -> Expr -> Expr -> Expr
sijoita muuttuja sisalto lauseke = 
    case lauseke of 
        (Var x) -> if x == muuttuja then sisalto else lauseke
        (Add y x) -> Add (sijoita muuttuja sisalto y) (sijoita muuttuja sisalto x)
        (Mul y x) -> Mul (sijoita muuttuja sisalto y) (sijoita muuttuja sisalto x)
        (Let um us x) -> sijoita um (sijoita muuttuja sisalto us) (sijoita muuttuja sisalto x)
        otherwise -> lauseke