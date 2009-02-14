module App.Persistent.Client.Message
    (
     Message(..),
     serializeMessage,
     unserializeMessage,
    ) where

import Data.Char
import Data.Either
import Data.Ratio
import Text.JSON
import Text.JSON.Types


data Message = KeyPress Char
             | NormalOutput String
             | ErrorOutput String
             | Exit Integer
             -- | Startup [(String, String)] [String]
               deriving (Show, Eq)

makeBasicMessage :: String -> String -> JSObject JSString
makeBasicMessage t v = toJSObject [("type", toJSString t),("value", toJSString v)]

-- write Messages
messageToJSON :: Message -> JSObject JSString
messageToJSON (KeyPress k) = makeBasicMessage "keyPress" (show $ ord k)

jsonForNetwork :: JSObject JSString -> String
jsonForNetwork obj = (encode obj) ++ "\r\n"

serializeMessage :: Message -> String
serializeMessage = (jsonForNetwork . messageToJSON)

-- read Messages

parseMessage_ (JSString t) (JSString v)
    | fromJSString t == "normalOutput" = Right $ NormalOutput (fromJSString v)
    | fromJSString t == "errorOutput"  = Right $ ErrorOutput  (fromJSString v)
parseMessage_ (JSString t) (JSRational _ v)
    | fromJSString t == "exit" = Right $ Exit (numerator v)
parseMessage_ _ _ = Left "Parse error: no pattern match"

parseMessage :: JSObject JSValue -> Either String Message
parseMessage json =
    case findNecessaryKeys (fromJSObject json) of
      Nothing -> Left "Invalid message content "
      Just (t, v) -> parseMessage_ t v

-- --
findNecessaryKeys :: [(String, a)] -> Maybe (a, a)
findNecessaryKeys obj = do
  t <- lookup "type" obj
  v <- lookup "value" obj
  return (t, v)

-- unserializeMessage :: String -> Either String Message
unserializeMessage str = do
  json <- resultToEither (decode str)
  parseMessage json
