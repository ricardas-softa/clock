module Main where
import Lib
import Control.Concurrent
import Control.Monad
import Data.Maybe
import System.Exit
import System.IO
import Data.Char (toLower)
import System.Console.ANSI

main :: IO ()
main = do
        hideCursor
        setTitle brand
        clearScreen
        size <- determineClockSize
        runClock (Config size) buildInitState

determineClockSize :: IO Int
determineClockSize = do
                        termSize <- getTerminalSize
                        case termSize of
                          Just (w,h) -> return $ round (fromIntegral (min w h) / 1.3)
                          Nothing -> return clockDefaultSize