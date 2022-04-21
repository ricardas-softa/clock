module Main where

import Lib
import System.Console.ANSI

main :: IO ()
main = clearScreen >> runClock (Config 31)