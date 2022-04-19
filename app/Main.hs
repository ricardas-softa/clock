module Main where

import Lib

main :: IO ()
main = let config = Config 31
           time = Time 10 10 15
   in display (render config (drawClock config time))