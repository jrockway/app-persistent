module Main where
import Network
import System.IO
import Control.Concurrent
import Control.Monad

import App.Persistent.Client.Message

startLoop :: IO a -> ( a -> IO () ) -> IO ThreadId
startLoop h p = do
  m <- newEmptyMVar
  let wait  = do { c <- h; putMVar m c; wait }
      parse = do { c <- takeMVar m; p c; parse }
  forkIO $ wait
  forkIO $ parse

handleNetwork :: String -> IO ()
handleNetwork line =
    case unserializeMessage line of
      Right message ->
          case message of
            NormalOutput str -> hPutStr stdout str
            ErrorOutput  str -> hPutStr stderr str
      Left error ->
          hPutStrLn stderr ("** Internal error: " ++ error)

sendMessage :: Handle -> Message -> IO ()
sendMessage server msg = hPutStrLn server $ serializeMessage msg

main = do
  putStrLn "Ready."
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

  server <- connectTo "localhost" (PortNumber 1234)
  hSetBuffering server NoBuffering

  startLoop (hGetChar stdin) (\c -> sendMessage server (KeyPress c))
  startLoop (hGetLine server) handleNetwork

  exit <- newEmptyMVar
  takeMVar exit
