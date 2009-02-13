module App.Persistent.Client.Message
    (
     Message(..),
     serializeMessage,
    ) where

import Text.JSON
import Text.JSON.Types
import Data.Char

data Message = KeyPress Char
             | Output
             -- | Startup [(String, String)] [String]
               deriving (Show, Eq)

makeBasicMessage :: String -> String -> JSObject JSString
makeBasicMessage t v = toJSObject [("type", toJSString t),("value", toJSString v)]

makeMessageJSON :: Message -> JSObject JSString
makeMessageJSON (KeyPress k) = makeBasicMessage "KeyPress" (show $ ord k)

jsonForNetwork :: JSObject JSString -> String
jsonForNetwork obj = (encode obj) ++ "\r\n"

serializeMessage :: Message -> String
serializeMessage = (jsonForNetwork . makeMessageJSON)
