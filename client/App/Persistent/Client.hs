module App.Persistent.Client
    (
     parseCmdLine
    )
    where

import Data.List

-- anything between +PC and -PC is for us, everything else is for them
parseCmdLine :: [String] -> ([String], [String])
parseCmdLine args =
    let (before, middle) = span (/= "+PC") args
        (inside, after) = span (/= "-PC") middle in
    (drop 1 inside, before ++ drop 1 after)
