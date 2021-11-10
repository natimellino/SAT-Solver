module Main where

import System.Environment (getArgs)
import Lib
import Parse
import CTL

main :: IO ()
main = do args <- getArgs
          contents <- readFile $ head args
          let model = func $ lexer contents
          print model
          
