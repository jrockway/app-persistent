module App.Persistent.Client
    (
     parseCmdLine
    ,getAppName
    ,getSocketPort
    )
    where

import System.Console.GetOpt
import Data.Maybe ( fromMaybe )
import Data.List
import Network
import System.Posix.User

data Flag = AppName String
          deriving (Read, Show, Ord, Eq)

options :: [OptDescr Flag]
options =
    [ Option ['n'] ["name"] (OptArg appname "APPNAME") "name of app to connect to"
    ]

defaultAppName = "pserver";

appname :: Maybe String -> Flag
appname = AppName . fromMaybe defaultAppName

-- anything between +PC and -PC is for us, everything else is for them
parseCmdLine' :: [String] -> ([String], [String])
parseCmdLine' args =
    let (before, middle) = span (/= "+PC") args
        (inside, after) = span (/= "-PC") middle in
    (drop 1 inside, before ++ drop 1 after)

parseCmdLine :: [String] -> IO ([Flag], [String])
parseCmdLine args = do
    let (our, their) = parseCmdLine' args
    ourParsed <- parseOurArgs our
    return ( ourParsed, their )

parseOurArgs :: [String] -> IO [Flag]
parseOurArgs args =
    case getOpt RequireOrder options args of
      (o,n,[]  ) -> return o
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: pclient +PC [OPTION...] -PC [SERVER ARGS...]"

getAppName :: [Flag] -> String
getAppName flags =
    let finder x = case x of
                     (AppName _) -> True
                     -- to be expanded later, presumably
        appflag = find finder flags in
    case appflag of
      Just (AppName name) -> name
      Nothing -> defaultAppName

getSocketPort :: [Flag] -> IO PortID
getSocketPort flags = do
    username <- getEffectiveUserName
    return (UnixSocket ("/tmp/pserver-" ++ username ++ "/" ++ (getAppName flags)))
