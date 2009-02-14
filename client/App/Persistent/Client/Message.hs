module App.Persistent.Client.Message
    (
     Message(..),
     serializeMessage,
     unserializeMessage,
    ) where

import Control.Monad
import Data.Char
import Data.Either
import Text.JSON
import Text.JSON.Types
import Text.JSON.String

data Message = KeyPress Char
             | NormalOutput String
             | ErrorOutput String
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
parseMessage :: [(String, JSString)] -> Either String Message
parseMessage obj =
    case findNecessaryKeys obj of
      Nothing -> Left "Invalid message content"
      Just (t, v) -> parseMessage_ t v

parseMessage_ :: String -> String -> Either String Message
parseMessage_ t v =
    case t of
      "normalOutput" -> Right (NormalOutput v)
      "errorOutput"  -> Right (ErrorOutput v)
      otherwise      -> Left ("Unknown message type '" ++ t ++ "'")

findNecessaryKeys :: [(String, JSString)] -> Maybe (String, String)
findNecessaryKeys obj = do
  t <- lookup "type" obj
  v <- lookup "value" obj
  return (fromJSString t, fromJSString v)

unserializeMessage :: String -> Either String Message
unserializeMessage str = do
  json <- resultToEither (decode str)
  parseMessage (fromJSObject json)
