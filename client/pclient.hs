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

parseInput :: ( Message -> IO () ) -> Char -> IO ()
parseInput callback c = do
  putStrLn ("Got input: " ++ show c)
  callback (KeyPress c)

sendMessage :: Handle -> Message -> IO ()
sendMessage server msg = hPutStrLn server $ serializeMessage msg

main = do
  putStrLn "Ready."
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

  server <- connectTo "localhost" (PortNumber 1234)
  hSetBuffering server NoBuffering

  startLoop (hGetChar stdin) (parseInput (sendMessage server))
  startLoop (hGetLine server) (putStrLn)

  exit <- newEmptyMVar
  takeMVar exit
