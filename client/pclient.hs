module Main where
import Control.Concurrent
import Control.Monad
import Network
import System.Exit
import System.IO

import App.Persistent.Client.Message

startLoop :: IO a -> ( a -> IO () ) -> IO ThreadId
startLoop h p = do
  m <- newEmptyMVar
  let wait  = do { c <- h; putMVar m c; wait }
      parse = do { c <- takeMVar m; p c; parse }
  forkIO $ wait
  forkIO $ parse

handleNetwork :: MVar Int -> String -> IO ()
handleNetwork exit line =
    case unserializeMessage line of
      Right message ->
          case message of
            NormalOutput str -> hPutStr stdout str
            ErrorOutput  str -> hPutStr stderr str
            Exit        code -> putMVar exit code
      Left error ->
          hPutStrLn stderr ("**Internal error: " ++ error)

sendMessage :: Handle -> Message -> IO ()
sendMessage server msg = hPutStrLn server $ serializeMessage msg

main = do
  putStrLn "Ready."
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  exit <- newEmptyMVar
  server <- connectTo "localhost" (PortNumber 1234)
  hSetBuffering server NoBuffering

  startLoop (hGetChar stdin) (\c -> sendMessage server (KeyPress c))
  startLoop (hGetLine server) (handleNetwork exit)

  exitCode <- takeMVar exit
  putStrLn $ "Bye (" ++ (show exitCode) ++ ")"
  exitWith $ case exitCode of
               0 -> ExitSuccess
               _ -> ExitFailure exitCode
