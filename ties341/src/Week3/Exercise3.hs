module Week3.Exercise3 where
import Control.Applicative

data ParseError = SomethingWentWrong
  deriving Show



newtype Parser a = Parser {getParser :: String -> Either ParseError (String, a)}
-- Luodaan funktori-instanssi
instance Functor Parser where
    fmap f (Parser x) = Parser $ \z -> 
        case x z of
            Left _ -> Left SomethingWentWrong
            Right (jalj, merkki) -> Right (jalj, f merkki)


instance Applicative Parser where
    pure x = Parser (\z -> Right (z,x))
    Parser f <*> Parser x = Parser (\merkkijono -> 
        case (f merkkijono) of 
            Left    _     -> Left SomethingWentWrong
            Right (m, a) -> getParser (fmap a (Parser x)) m
            )
 
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
    
anySingleBut ::  Char -> Parser Char
anySingleBut merkki = (satisfy . (/=)) merkki
 
oneOf :: [Char] -> Parser Char
oneOf merkit =  satisfy (flip elem merkit)
  
noneOf :: [Char] -> Parser Char
noneOf merkit =  satisfy (flip notElem merkit)
  

chunk :: String -> Parser String
chunk ""     = Parser $ \input -> Right (input, "")
chunk (x:merkkijono) = (:) <$> single x <*> chunk merkkijono

    


   

