import Network
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import System.IO
import Text.Printf
import Data.Char
import Data.List
import Text.HTML.TagSoup
import Data.Text (splitOn, pack, unpack)
import System.Exit

main = do
    bot <- connect
    identify bot
    listen bot

filterXmlTags :: [Tag str] ->  [Tag str]
filterXmlTags [] = []
filterXmlTags (tag@(TagOpen _ _):xs) = tag : filterXmlTags xs
filterXmlTags (_:xs) = filterXmlTags xs

weatherurl = "http://www.google.com/ig/api?weather="

getWeatherResult :: Bot ->  Message ->  String ->  IO ()
getWeatherResult bot message city = do
    html <- getHtmlContent (weatherurl ++ city)
    let tags = parseTags html
    if (length (sections (~== "<problem_cause>") tags) > 0)
        then
            respond (getHandle bot) message "Such city does not exist."
        else do
            let datas = map (fromAttrib "data") $ filterXmlTags tags
            respond (getHandle bot) message (
                printf "City: %s, Condition: %s, Temperature: %sC, %s, %s" city (datas !! 12) (datas !! 14) (datas !! 15) (datas !! 17))

wrongWeatherCommand = "Example: \"!weather Kaunas\""

checkWeather :: Bot ->  Message ->  IO ()
checkWeather bot message = do
    let userInput = splitOn' " " (getMessage message)
    if length userInput == 2
        then do
            let city = init $ userInput !! 1
            getWeatherResult bot message (init $ userInput !! 1)
        else
            respond (getHandle bot) message wrongWeatherCommand

server = "irc.data.lt"
port' = PortNumber 6667
nick = "jelly"
chan = "#haskell"

forever :: (Monad m) => m a ->  m b
forever f = f >> forever f

data Bot = Bot { getHandle :: Handle }

data Message = Message {
        getSender :: String,
        getType :: String,
        getReceiver :: String,
        getMessage :: String } | Empty deriving (Show)

createMessage :: String ->  Message
createMessage s
    | length (splitOn' " " s) > 3   = let spl = (splitOn' " " s) in
                                        Message
                                            (tail $ head spl)
                                            (head $ tail spl)
                                            (head $ drop 2 spl)
                                            (filterFromBaltic $ tail $ concat $ intersperse " " $ drop 3 spl)

    | otherwise                         = Empty


filterFromBaltic :: String ->  String
filterFromBaltic source = map replace source

replace :: Char -> Char
replace c = case c of
    'ą' -> 'a'
    'č' -> 'c'
    'ę' -> 'e'
    'ė' -> 'e'
    'į' -> 'i'
    'š' -> 's'
    'ų' -> 'u'
    'ū' -> 'u'
    'ž' -> 'z'
    _   -> c

splitOn' :: String ->  String ->  [String]
splitOn' spl source = map unpack (splitOn (pack spl) (pack source))

listen :: Bot ->  IO ()
listen bot = forever $ do
    let h = getHandle bot
    s <- hGetLine h
    checkPing bot s
    process bot (createMessage s)
    putStrLn s

process :: Bot ->  Message ->  IO ()
process _ Empty = return ()

process bot message = case map toLower $ getMessage message of
    msg | isPrefixOf "!exchange" msg    ->  doExchange bot message
        | isPrefixOf "!weather" msg     ->  checkWeather bot message
        | msg == "!commands\r"          ->  respond (getHandle bot) message "Commands: !exchange !weather !fml"
                                                                            -- !gtfo is hidden intentionally
        | msg == "!fml\r"               ->  getRandomFml bot message
        | msg == "!gtfo\r"              ->  exitWith ExitSuccess
        | otherwise                     ->  return ()

exchangeurl :: String
             --http://www.google.com/ig/calculator?hl=en&q=100EUR%3D%3FDKK
exchangeurl = "http://www.google.com/ig/calculator?hl=en&q="

fillExchangeUrl :: [String] ->  String
fillExchangeUrl params =
    exchangeurl ++
    (params !! 0) ++
    (params !! 1) ++
    "%3D%3F" ++
    (init (params !! 3))

wrongExchangeCommand :: String
wrongExchangeCommand = "Example: \"!exchange 100 eur to dkk\""

getExchangeJSONValue :: String ->  String
getExchangeJSONValue json = init (splitOn' "\"" json) !! 1

getExchangeResult :: String ->  String
getExchangeResult json = do
    let parts = splitOn' "," json
    if parts !! 2 == "error: \"4\""
        then wrongExchangeCommand
        else do
            getExchangeJSONValue (parts !! 0) ++ " = " ++ getExchangeJSONValue (parts !! 1)

doExchange :: Bot ->  Message ->  IO ()
doExchange bot message = do
    let userInput = splitOn' " " (getMessage message)
    let h = getHandle bot
    if length userInput == 5
        then do
            html <- getHtmlContent $ fillExchangeUrl $ tail userInput
            let result = (getExchangeResult html)
            respond h message result
        else respond h message (wrongExchangeCommand)

fmlurl :: String
fmlurl = "http://www.fmylife.com/random"

getRandomFml :: Bot ->  Message ->  IO ()
getRandomFml bot message = do
    let h = getHandle bot
    html <- getHtmlContent fmlurl
    let fml = extractFml html
    respond h message fml

getHtmlContent :: String ->  IO String
getHtmlContent url = do
    result <- simpleHTTP (getRequest url)
    getResponseBody result

extractFml :: String ->  String
extractFml html = head $ splitOn' "#" $ innerText $ head $ sections (~== "<div class=\"post article\"") (parseTags html)

checkPing :: Bot ->  String ->  IO ()
checkPing bot s = case ping s of
                    True ->  pong bot s
                    False ->  return ()

ping :: String ->  Bool
ping = isPrefixOf "PING :"

pong :: Bot ->  String ->  IO ()
pong bot s = write (getHandle bot) "PONG" (drop 4 s)

identify :: Bot ->  IO ()
identify bot = do
    let h = getHandle bot
    write h "NICK" nick
    write h "USER" (nick ++ " 0 * :jelly")
    write h "JOIN" chan

respond :: Handle ->  Message ->  String ->  IO ()
respond h m s = write h ("PRIVMSG " ++ (getReceiver m)) s

write :: Handle ->  String ->  String ->  IO ()
write h s t = do
    hPrintf h   "%s %s\r\n" s t
    printf      "> %s %s\n" s t

connect :: IO Bot
connect = connectTo server port' >>= \h ->  hSetBuffering h NoBuffering >> hSetEncoding h utf8 >> return (Bot h)

