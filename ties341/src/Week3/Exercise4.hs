module Week3.Exercise4 where

import Control.Applicative
import Data.List (intersect)
import Week2.Exercise3
import Week3.Exercise3
import Data.Char


type Unit = String
type Record = [Unit]
type Group = [Record]

fromEither :: Either a a -> a
fromEither (Left x) = x
fromEither (Right x) = x

showsEscaped :: String -> ShowS
showsEscaped [] = id
showsEscaped (c : cs)
  | c == '"' = mappend (replicate 2 '"') . showsEscaped cs
  | otherwise = (c :) . showsEscaped cs

showsQuoted :: String -> ShowS
showsQuoted cs = ('"' :) . showsEscaped cs . ('"' :)

showsGroup :: Group -> ShowS
showsGroup xsss = flip (foldr (\ xss ->
  fromEither . runJoin . flip (foldr (\ xs css -> let
    f = case intersect ['\n', '\r', '"', ','] xs of
      [] -> mappend xs
      _ : _ -> showsQuoted xs
    x = case css of
      Join (Left cs) -> ',' : cs
      Join (Right cs) -> '\n' : cs in
    Join (Right (f x)))) xss . Join . Left)) xsss

--runParser :: Parser a -> String -> Either ParseError (String, a)
--runParser p mjono = getParser p mjono 
{-
Otetaan string ->
    - otetaan char
    - jos on quote -> niin luetaan chunk, 
        kunnes tulee taas quote merkki (tästä unit ja lisätään se recordiin)
         tai kunnes tulee rivin vaihto (tästä unit, se recordiin, ja 
         record groupiin, aloitetaan uusi record)
        -> tästä unit
        -> lisätään unit
    - jos on (,) , edellisistä unit ja lisätään recordiin, siirrytään seuraavaan
    - jos ei, lisätään käsittelyssä olevaan unitiin 

-}
{-
yks :: Unit
yks = unQuote <|> quote
(<|>) = undefined

char = undefined 
(<*>) = undefined
quote = char '"' <*> many (anySingleBut '"') <*> char '"'

unQuote =  many (anySingleBut '"') <*> yks
-}

unQuoted = many (anySingleBut ',') 

quoted_left = (single '"') *> (many (anySingleBut '"')) 
quoted_both =quoted_left <* (single '"')
--yks :: Unit
--yks = quoted <|> yks 
--(<|>) = undefined

unit :: Parser Unit
--unit = unQuote <|> quoted
unit = many (anySingleBut ',') 

record :: Parser Record
record =  many ((single ',') *> sev_unit)

sev_unit = (many (single ',') *> unit) <|> unit 

-- Tähän tarttuu string, jossa ei ole pilkkuja.
str =  many (anySingleBut ',') 
eol = single '\n'


--group :: Parser Group
--group = many record

parseProgram p s = getParser p s
     --Right (x,y) -> Just (y)
     --Left _ ->   Nothing


