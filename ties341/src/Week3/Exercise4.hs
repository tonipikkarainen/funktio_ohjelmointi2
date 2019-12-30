module Week3.Exercise4 where

import Control.Applicative
import Data.List (intersect)
import Week2.Exercise3
import Week3.Exercise3
import Data.Char


type Unit = String
type Record = [Unit]
type Group = [Record]


-- CSV - tiedoston parsiminen
--
-- 18.11.2019
-- Toni Pikkarainen

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






