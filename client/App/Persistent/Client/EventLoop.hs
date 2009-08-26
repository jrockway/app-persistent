module App.Persistent.Client.EventLoop
    (
     EventHandler(..),
     startLoop
    )
    where

import Control.Concurrent
import Control.Monad
import System.IO
import System.IO.Error

data EventHandler a =
    EventHandler { onRead :: ( a -> IO () ), onEof :: IO () }

startLoop :: MVar Int -> IO a -> EventHandler a -> IO [ThreadId]
startLoop exit reader handler  = do
  input <- newChan :: IO (Chan (Maybe a))
  let wait = do
        c <- liftM Just reader `catch` eofHandler
        writeChan input c
        case c of Just _  -> wait
                  Nothing -> return ()

      parse = do
        c <- readChan input
        case c of
          Just c -> ((onRead handler c) `catch` killProgram) >> parse
          Nothing -> return ()

      eofHandler e = ( if isEOFError e
                       then onEof handler
                       else killProgram e ) >> return Nothing

      killProgram e = do
               hPutStrLn stderr $ "Error: " ++ (show e)
               putMVar exit 255
  pid <- forkIO parse
  wid <- forkIO wait
  return [pid,wid]
