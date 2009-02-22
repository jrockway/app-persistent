{-# LANGUAGE DeriveDataTypeable #-}

module App.Persistent.Client.Message
    (
     Message(..),
     StandardDescriptor(..),
     serializeMessage,
     unserializeMessage,
    ) where

import Data.Char
import Data.Data
import Data.Either
import Data.Ratio
import Text.JSON.Generic

data StandardDescriptor = StdIn | StdOut | StdErr | OtherDescriptor
                        deriving (Typeable, Data, Show, Eq)

               -- messages we send
data Message = KeyPress Char
             | EndOfFile StandardDescriptor
             | ProgramName String
             | Environment [(String, String)]
             | CommandLineArgs [String]
             | WorkingDirectory String
               -- messages we receive
             | NormalOutput String
             | ErrorOutput String
             | Exit Int
               deriving (Typeable, Data, Show, Eq)

serializeMessage :: Message -> String
serializeMessage = encodeJSON

unserializeMessage :: String -> Message
unserializeMessage = decodeJSON

