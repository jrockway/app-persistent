module Main where
import Control.Concurrent
import Directory
import Network
import System.Environment
import System.Exit
import System.IO

import App.Persistent.Client.Message
import App.Persistent.Client.EventLoop
import App.Persistent.Client

handleNetwork :: MVar Int -> String -> IO ()
handleNetwork exit line =
    case unserializeMessage line of
      NormalOutput str -> hPutStr stdout str
      ErrorOutput  str -> hPutStr stderr str
      Exit        code -> putMVar exit code

sendMessage :: Handle -> Message -> IO ()
sendMessage server msg = hPutStrLn server (serializeMessage msg)

sendStartupInfo :: Handle -> [String] -> IO ()
sendStartupInfo server args = do
  p <- getProgName
  e <- getEnvironment
  d <- getCurrentDirectory

  mapM (sendMessage server)
           [ ProgramName p
           , CommandLineArgs args
           , Environment e
           , WorkingDirectory d
           ]
  return ()

main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  cmdLineArgs <- getArgs
  let (ourArgs, theirArgs) = parseCmdLine cmdLineArgs

  exit <- newEmptyMVar
  server <- connectTo "localhost" (PortNumber 1234)
  hSetBuffering server NoBuffering

  sendStartupInfo server theirArgs

  startLoop (hGetLine server)
            EventHandler { onRead = handleNetwork exit,
                           onEof  = exitWith ExitSuccess }

  startLoop (hGetChar stdin)
            EventHandler { onRead = \c -> sendMessage server (KeyPress c),
                           onEof  = sendMessage server (EndOfFile StdIn) }

  exitCode <- takeMVar exit
  exitWith $ case exitCode of
               0 -> ExitSuccess
               _ -> ExitFailure exitCode
