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

startLoop :: IO a -> EventHandler a -> IO ()
startLoop reader handler  = do
  input <- newChan :: IO (Chan (Maybe a))
  let wait = do
        c <- liftM Just reader `catch` eofHandler
        writeChan input c
        case c of Just _ -> wait; Nothing -> return ();

      parse = do
        c <- readChan input
        case c of
          Just c -> onRead handler c >> parse
          Nothing -> return ()

      eofHandler e = if isEOFError e
                     then onEof handler >> return Nothing
                     else ioError e
  forkIO parse
  forkIO wait
  return () -- no need to "leak" the ThreadId
