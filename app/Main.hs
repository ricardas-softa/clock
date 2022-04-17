module Main where

import Lib

main :: IO ()
main = let config = Config 31
           time = Time 1 16 32
   in display (render config (drawClock config time))