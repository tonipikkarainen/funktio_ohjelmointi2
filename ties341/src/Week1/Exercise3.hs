-- Tekijä: Toni Pikkarainen
module Week1.Exercise3 where
import Control.Monad.State
import Data.List
--import GHC.Exts



--class Transaction k m a where
--  recall :: k -> m a
--  store :: k -> a -> m ()

data Cursor a = WayBefore | Before [a] | At [a] a [a] | After [a] | WayAfter

data Transaction' k m a = Transaction'
  {
    recall2 :: k -> m a
    ,store2 :: Eq' Int -> k -> a -> m ()
  }

data Eq' a = Eq' { equ :: a -> a -> Bool  }

data Ord' a = Ord' {comp :: a -> a -> Ordering,
                    (<) :: a -> a -> Bool,
                    (<=) :: a -> a -> Bool,
                    (>) :: a -> a -> Bool,
                    (>=) :: a -> a -> Bool,
                    max :: a -> a -> a,
                    min :: a -> a -> a}

intOrd :: Ord' Int
intOrd = Ord' comp s se l le max min where
          comp x y 
            | x > y = GT
            | x < y = LT
            | x == y = EQ
          s = (<)
          se = (<=)
          l = (>)
          le = (>=)
          max x y 
            | x > y = x
            | otherwise = y
          min x y 
            | x < y = x
            | otherwise  = y



intEq :: Eq' Int
intEq =  Eq' {equ = (==)}


-- Tässä toteutettu kolmelle luokalle
-- vaaditut muunnokset tyypeiksi. Samaa ideaa voitaisiin soveltaa
-- muihinkin edellä mainittuihin tyyppiluokkiin.

transIntIOString :: Transaction' Int IO String
transIntIOString = Transaction' 
  (  
    let recall2 i =  do
        s <- readFile database
        case navigate intOrd intEq  i (lines s) of
          At _ c _ -> pure c
          _ -> ioError (userError "Invalid line number")
     in recall2
  ) (let store2 eq i x  = do
            s <- readFile database
            case navigate intOrd  intEq i (lines s) of
              Before rs -> writeFile database
                (unlines (x : rs))
              At ls _ rs -> writeFile database
                (unlines (ls <> (x : rs)))
              After ls -> writeFile database
                (unlines (ls <> [x]))
              _ -> ioError (userError "Invalid line number")
       in store2       
  )

-- Täällä jäi let:stä pois konkreettiset tyypit
-- ja se aiheutti aluksi ongelmia. Sampsan opastuksella
-- laitoin sitten konkreettiset tyypit paikalleen.
transIntStateString :: Transaction' Int (State (Int -> String)) String
transIntStateString = Transaction' 
              (
                let
                  recall2 :: Int -> State (Int -> String) String 
                  recall2 i = gets $ \ f -> f i 
                in recall2
              )
              (
                let 
                  store2 :: Eq' Int ->  Int -> String -> State (Int -> String) ()
                  store2 eq i x = modify $ \ f j -> if (equ eq) j i then x else f j
                in  store2
              )

navigate :: Ord' Int ->  Eq' Int ->  Int -> [a] -> Cursor a
navigate ord eq n xs = case (ord comp) n (pred 0) of
  LT -> WayBefore
  EQ -> Before xs
  GT -> let
    f :: Int -> [a] -> Cursor a
    f p []
      | (equ eq) p 0 = After []
      | otherwise = WayAfter
    f p (z : zs)
      | (equ eq) p 0 = At [] z zs
      | otherwise = case f (pred p) zs of
        At bs a as -> At (z : bs) a as
        After bs -> After (z : bs)
        c -> c in
    f n xs

database :: FilePath
database = "/tmp/database"

--instance Transaction Int IO String where
--  recall i = do
--    s <- readFile database
--    case navigate i (lines s) of
--      At _ c _ -> pure c
--      _ -> ioError (userError "Invalid line number")
--  store i x = do
--    s <- readFile database
--    case navigate i (lines s) of
--      Before rs -> writeFile database
--        (unlines (x : rs))
--      At ls _ rs -> writeFile database
--        (unlines (ls <> (x : rs)))
--      After ls -> writeFile database
--        (unlines (ls <> [x]))
--      _ -> ioError (userError "Invalid line number")

--instance Transaction Int (State (Int -> String)) String where
--  recall i = gets $ \ f -> f i
--  store i x = modify $ \ f j -> if j == i then x else f j

work :: IO String
work = do
  (store2 transIntIOString) intEq (0 :: Int) "zero"
  (store2 transIntIOString) intEq (1 :: Int) "one"
  (store2 transIntIOString) intEq (2 :: Int) "two"
  x <- (recall2 transIntIOString) (0 :: Int)
  y <- (recall2 transIntIOString) (1 :: Int)
  z <- (recall2 transIntIOString) (2 :: Int)
  pure (intercalate " " [x, y, z])

mock :: String
mock = let
  f :: Int -> String
  f _ = mempty in
  flip evalState f $ do
  (store2 transIntStateString) (intEq) (0 :: Int) "zero"
  (store2 transIntStateString) (intEq)   (1 :: Int) "one"
  (store2 transIntStateString) (intEq)  (2 :: Int) "two"
  x <- (recall2 transIntStateString) (0 :: Int)
  y <- (recall2 transIntStateString) (1 :: Int)
  z <- (recall2 transIntStateString) (2 :: Int)
  pure (intercalate " " [x, y, z])