--{-# LANGUAGE OverloadedStrings #-}
module Week5.Exercise4 where
import Data.Text hiding (length,head)
import Data.Text.Read
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad.Trans.Class
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Control.Exception as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Network.HTTP.Types.Status (statusCode)
import           Data.CaseInsensitive  ( CI )
import qualified Data.CaseInsensitive as CI
import Control.Monad.Trans.State.Strict as State
import Control.Monad.Trans.Reader as Reader
import System.Console.ANSI
import Control.Concurrent
import System.Directory







--main :: IO ()
--main = httpTest


type Parser = Parsec Void String


decimal' :: Parser [Char]
decimal' = many (oneOf ['1'..'9'])

onkoBool :: Parser [Char]
onkoBool = chunk "True" <|> chunk "False"

-- parse numbers "" "11,2,43"
-- Parsitaan url
parseUrl' :: Parser Text
parseUrl' = do
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
    u <- parseUrl'
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


-- TODO: mieti polku tiedostolle!!!
-- TODO: responsetimeout:in säätäminen
httpTest :: String ->  IO ()
httpTest s = do
  s <- readFile s--"/Users/tonipikkarainen/master_degree/funktio2/ties341/src/Week5/addresses.cfg"
  seq (length s) (pure ())
  let hosts = (parse parseManyHost "" s)  
  case hosts of 
    Right (x:xs) -> 
        do
            manager <- newManager tlsManagerSettings
            requestfunktio (x:xs) manager
    Left _ -> do 
        print "Something wrong"
        setSGR [Reset]
   




requestfunktio :: [(Text,Text,Text)] -> Manager -> IO ()
requestfunktio [] man = putStrLn "----"
requestfunktio (x:xs) man = do
    let (url,number,color) = x
    if (unpack color) == "True" then do
        setSGR [SetColor Foreground Vivid Blue] -- väritys
        (yritykset, koodi) <- runStateT (tutki man 0 (url,number,color)) 0
        putStrLn ((unpack url) ++ (" - yritykset: ") ++ (show yritykset) ++ (" - Status: ") ++ (show koodi))
        setSGR [Reset]
        else do
            (yritykset, koodi) <- runStateT (tutki man 0 (url,number,color)) 0
            putStrLn ((unpack url) ++ (" - yritykset: ") ++ (show yritykset) ++ (" - Status: ") ++ (show koodi))
    requestfunktio xs man  

-- tutkitaan statuskoodia,
-- pidetään tilassa nykyinen statuskoodi, tilan
-- arvo on askelten määrä.
tutki :: Manager -> Int -> (Text, Text, Text) -> StateT Int (IO) Int
tutki man toistot (url,numberLimit,color) = do
    let raja = read $ unpack numberLimit :: Int 
    if toistot >= raja then pure ( toistot ) 
        else do 
            request <- lift (parseRequest (unpack url))
            let req = request 
                    { method = C.pack "HEAD" 
                    ,requestHeaders = [makeHeader "Accept" "*/*", makeHeader "User-Agent" "ties341/0.0.0"]}
            stat <- lift ( eval req man )
            if stat == 200 || stat == (-2) then do
                put (stat)
                pure ( toistot + 1 ) 
                else do
                    put (stat)
                    lift (threadDelay 500000) -- odotusaika
                    tutki man (toistot + 1) (url,numberLimit,color)
-- Tehdään request ja palautetaan statuskoodi
-- Jos ei saada yhteyttä ja tulee poikkeus, palautetaan -1.
eval :: Request -> Manager -> IO Int                   
eval q m = do
    E.catch (do 
                b <- (httpNoBody q m)
                pure (statusCode (responseStatus b))) $ \ e -> seq (e :: HttpException) (pure (600))

-- tän vois sijoittaa koodiin?
makeHeader x y = (CI.mk $ C.pack x, C.pack y)
 