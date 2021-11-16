module Main where

import System.Environment (getArgs)
import Control.Monad.IO.Class
import Lib
import Parse
import CTL
import Eval

main :: IO ()
main = do args <- getArgs
          contents <- readFile $ head args
          let model = func $ lexer contents
          let sts = eval model
          print model
          print sts
          
