module Lib
    ( printTime
    , getTime
    ) where

import Data.Time

printTime :: IO ()
printTime = do
    time <- getTime
    print time

getTime = getCurrentTime