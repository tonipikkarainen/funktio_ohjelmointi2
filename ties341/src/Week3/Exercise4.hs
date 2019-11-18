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


-- Tästä alkaa itse tehty parseri:

-- Pienin yksikkö - pilkkujen välistä otettu merkkijono.
-- Voi olla lainausmerkkien sisällä tai ilman lainausmerkkejä.
unit :: Parser Unit
unit = quoted <|> many (noneOf [',','\n','\r', '"'])


-- Tässä otetaan kiinni lainausmerkkien välissä olevat merkkijonot.
-- Siellä saa olla myös merkkejä ',' , '""' , '\n' , '\r' 
-- eivätkä nämä pilkut tai rivinvaihdot katkaise yksikköä.
quoted :: Parser Unit
quoted = single '"' *> (mconcat <$> (many ((\x y -> x:y:[]) <$> single '"' <*> single '"' <|>  ((:[]) <$> anySingleBut '"')  ))) <* single '"' 

-- Luodaan rivi:
-- Alussa pitää olla yksi unit, sitten pitää tulla 0 tai enemmän kokonaisuuksia
-- ',' + unit
record :: Parser Record
record =  (:) <$> unit <*> (many ( (single ',') *> unit )) 

-- Luetaan koko tiedosto:
-- sisältää monta riviä ja rivi sisältää monta yksikköä.
-- Alkuun 0 tai enemmän rivejä joiden lopussa on rivinvaihto ("record_sep").
-- Yhdistetään ne loppuun vaadittavaan riviin, jossa on 
-- tiedostonloppu ("eof") viimeisenä.
group :: Parser Group
group = (++) <$> (many (record <* record_sep)) <*> ((:) <$> (record <* eof) <*> pure [])

-- Rivinvaihto
-- Joko '\n' tai '\r''\n'
record_sep :: Parser Char
record_sep = single ('\n') <|> ((single '\r') <* (single '\n'))

-- Testausjono
testijono = ",,testaan,1,1,,,,\" j,j,j\"\"hello\"\",,,\",\r\n,test,\"i,i,i,i\""

readTxtFile f = do
  s <- readFile f
  seq (length s) (pure ())
  print (getParser group s)






