module Week5.Exercise4 where
import Data.Text hiding (length)
import Data.Text.Read
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
--import Text.Megaparsec.String


main :: IO ()
main = undefined

data HostData = HostData { url :: Text,
             numberOfRepeats :: Int, useColor :: Bool } 
             deriving (Eq, Show)

type Parser = Parsec Void String

dot :: Parser Char
dot = single '.'

--numbers = decimal' `sepBy` char ','

decimal' :: Parser [Char]
decimal' = many (oneOf ['1'..'9'])

onkoBool :: Parser [Char]
onkoBool = chunk "True" <|> chunk "False"

-- parse numbers "" "11,2,43"
-- Parsitaan url
parseUrl :: Parser Text
parseUrl = do
    single '{'
    space
    chunk "url"
    space
    single '='
    space
    single '\"'
    r <- (many (anySingleBut '\"'))
    single '\"'
    space
    single ','
    return (pack r)
-- toistojen lukumäärä
parseRepeats :: Parser Text
parseRepeats = do
    space
    chunk "numberOfRepeats"
    space
    single '='
    space
    r <- decimal' 
    space
    single ','
    return (pack r)
-- väritys
parseColor :: Parser Text
parseColor = do
    space
    chunk "useColor"
    space
    single '='
    space
    r <- onkoBool 
    space
    single '}'
    return (pack r)
-- parsitaan yksi hosti
parseHost :: Parser (Text, Text, Text)
parseHost = do
    space
    u <- parseUrl
    n <- parseRepeats
    c <- parseColor
    space
    many (single ',')
    return (u,n,c)
-- parsitaan koko json-tiedosto
parseManyHost :: Parser [(Text, Text, Text)]
parseManyHost = do
    space 
    single '{'
    r <- many parseHost
    single '}'
    space
    eof
    return r

--testijson = "{{url = \"https://example.com/\",numberOfRepeats = 5, useColor = True}{url = \"https://example.com/\",numberOfRepeats = 5, useColor = True}}"

-- TODO: parsimisen virhe pitäis ottaa kiinni paremmin
-- Eli jos tulee parsimisesta Left ...
readTxtFile f = do
    s <- readFile f
    seq (length s) (pure ())
    print (parse parseManyHost "" s)

