module Week3.Exercise3 where
import Control.Applicative

data ParseError = SomethingWentWrong
  deriving Show


-- rupesi toimimaan kun tämän vaihtoi newtypeksi - miksi???!!!????!!!!
--
newtype Parser a = Parser {getParser :: String -> Either ParseError (String, a)}
-- Luodaan funktori-instanssi
instance Functor Parser where
    fmap f (Parser x) = Parser $ \z -> 
        case x z of
            Left _ -> Left SomethingWentWrong
            Right (jalj, merkki) -> Right (jalj, f merkki)

{-
(g . x) where
        g (Left z) = Left z
        g (Right (z,y)) = Right (z, f y) -}
instance Applicative Parser where
    pure x = Parser (\z -> Right (z,x))
    Parser f <*> Parser x = Parser (\merkkijono -> 
        case (f merkkijono) of 
            Left    _     -> Left SomethingWentWrong
            Right (m, a) -> getParser (fmap a (Parser x)) m
            )
  {-case (x m) of 
                Right (s,k)     -> Right (s,(a k))
                Left z          -> Left z -}  

instance Alternative Parser where
    empty = Parser (\z -> Left SomethingWentWrong)
    Parser x <|> Parser y = Parser $ \z -> 
        case x z of 
            Left _ -> (y z)
            result      -> result



eof :: Parser ()
eof = Parser $ \merkkijono -> 
    if merkkijono == "" then Right (merkkijono,()) else Left SomethingWentWrong

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \input -> 
    case input of 
        (x:xs) -> if (f x) then Right (xs,x) else Left SomethingWentWrong
        []     ->  Left SomethingWentWrong

item :: Parser Char
item = Parser $ \input -> case input of
    [] -> Left SomethingWentWrong
    (x:xs) -> Right (xs, x)

single :: Char -> Parser Char
single merkki = (satisfy . (==)) merkki
    --Parser $ \(x:xs) -> 
    --if (merkki ==  x) then Right (xs,x) else Left SomethingWentWrong

anySingleBut ::  Char -> Parser Char
anySingleBut merkki = (satisfy . (/=)) merkki
    --Parser $ \(x:xs) -> 
    --if (merkki /=  x) then Right (xs,x) else Left SomethingWentWrong

oneOf :: [Char] -> Parser Char
oneOf merkit =  satisfy (flip elem merkit)
    --Parser $ \(x:xs) -> 
    --if any ((==) x) merkit  then Right (xs,x) else Left SomethingWentWrong


noneOf :: [Char] -> Parser Char
noneOf merkit =  satisfy (flip notElem merkit)
    --Parser $ \(x:xs) -> 
    --if any ((==) x) merkit  then Left SomethingWentWrong else Right (xs,x)


chunk :: String -> Parser String
chunk ""     = Parser $ \input -> Right (input, "")
chunk (x:merkkijono) = (:) <$> single x <*> chunk merkkijono

    


    {-
    f z 
        | (length z) < length merkkijono = Left SomethingWentWrong
        | take (length merkkijono) z == merkkijono = Right (drop (length merkkijono) z, merkkijono)
        | otherwise = Left SomethingWentWrong-}
    --(<$) :: Functor f => a -> f b -> f a infixl 4
--(<*>) :: f (a -> b) -> f a -> f b infixl 4
--
--(*>) :: f a -> f b -> f b infixl 4
--
--(<*) :: f a -> f b -> f a infixl 4
--
--some v = (:) <$> v <*> many v
--many v = some v <|> pure []
--some :: f a -> f [a]
--many :: f a -> f [a]
--
--optional :: Alternative f => f a -> f (Maybe a)





