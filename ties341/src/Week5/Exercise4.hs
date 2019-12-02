{-# LANGUAGE FlexibleContexts #-}
module Week5.Exercise4 where
import Data.Text hiding (length,head)
import Data.Text.Read
import Text.Megaparsec
import Text.Read
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
import Control.Monad.State.Strict as S
import Control.Monad.IO.Class
import System.Console.ANSI
import Control.Concurrent

-- Testataan, mikä statuskoodi palvelimelta saadaan
-- vastauksena http - HEAD -requestiin.
-- Tekijä: Toni Pikkarainen
-- 2.12.2019

-- TODO: Int:n lukeminen tiedostosta ylivuodon käsittelyllä.
-- nyt toteutettu readmaybellä - ylivuoto ei tässä aiheuta
-- ongelmia sillä negatiivinen luku pysäyttää kyseisen funktion
-- suorituksen.



-- Anna kysyttäessä polku konfiguraatiotiedostoon, josta
-- luetaan tutkittavat palvelut.
main :: IO ()
main  = do
  putStrLn "Anna luettavan tiedoston polku:" 
  polku <- getLine
  E.catch (do 
    s <- readFile polku
    seq (length s) (pure ())
    let hosts = (parse parseManyHost "" s)
    case hosts of 
        Right (x:xs) -> 
            do
                manager <- newManager tlsManagerSettings
                requestfunktio (x:xs) manager
        Left _ -> do 
            print "Something wrong"
            setSGR [Reset]) $ \e -> seq (e :: E.SomeException) (putStrLn "Tiedostoa ei löydy")
  
   

type Parser = Parsec Void String


decimal' :: Parser [Char]
decimal' = many (oneOf ['1'..'9'])

onkoBool :: Parser [Char]
onkoBool = chunk "True" <|> chunk "False"

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


-- TODO: responsetimeout:in säätäminen



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
tutki :: (MonadState (Int) m, MonadIO m) => Manager -> Int -> (Text, Text, Text) -> m Int
tutki man toistot (url,numberLimit,color) = do
    let x = readMaybe $ unpack numberLimit :: Maybe Int
    let raja = if x == Nothing then 1 else case x of (Just y) -> y -- voi tulla ylivuoto liian isolla syötteellä mutta tässä tapauksessa ei aiheuta ongelmia.
    if toistot >= raja then pure ( toistot )  -- jos raja menee negatiiviseksi tämä pysähtyy heti.
        else do 
            request <- liftIO (parseRequest (unpack url))-- Tässä ei ilmeisesti päästä lift:stä täysin eroon?
            let req = request 
                    { method = C.pack "HEAD" 
                    ,requestHeaders = [(CI.mk $ C.pack "Accept", C.pack  "*/*") ,
                     (CI.mk $ C.pack "User-Agent",  C.pack "ties341/0.0.0")]}
            stat <- liftIO ( eval req man )-- eikä tässä
            if stat == 200 || stat == (600) then do
                S.put (stat)
                pure ( toistot + 1 ) 
                else do
                    S.put (stat)
                    liftIO (threadDelay 500000)-- eikä tässä
                    tutki man (toistot + 1) (url,numberLimit,color)

-- Tehdään request ja palautetaan statuskoodi
-- Jos ei saada yhteyttä ja tulee poikkeus, palautetaan -1.
eval :: Request -> Manager -> IO Int                   
eval q m = do
    E.catch (do 
                b <- (httpNoBody q m)
                pure (statusCode (responseStatus b))) $ \ e -> seq (e :: HttpException) (pure (600))


