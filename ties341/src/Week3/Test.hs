module Test where
import Data.Char
import Control.Applicative

newtype Parser a = P (String -> [(a,String)])


parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
    []      -> []
    (x:xs)  -> [(x,xs)])


instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case parse p inp of
                          []        -> []
                          [(v,out)] -> [(g v,out)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v,inp)])
  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of
                            []        -> []
                            [(g,out)] -> parse (fmap g px) out)

instance Alternative Parser where
  empty = P (\cs -> [])
  p <|> q =  P $ \s ->
    case (parse p s) of
      [] -> (parse q s)
      _ -> parse p s

--combine p q = P (\s -> parse p s ++ parse q s)


dropMiddle :: Parser (Char,Char)
dropMiddle =
  pure selector <*> item <*> item <*> item
  where selector x y z = (x,z)      

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item `bind` \c ->
  if p c
  then pure c
  else (P (\cs -> []))

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = P $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

char c = satisfy (c ==)
