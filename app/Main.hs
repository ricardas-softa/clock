module Main where

import Lib
import System.Console.ANSI

main :: IO ()
main = let config = Config 31
           time = Time 10 10 15
   in clearScreen >> display (render config (drawClock config time))