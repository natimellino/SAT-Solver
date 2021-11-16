module Main where

import System.Environment (getArgs)
import Control.Monad.IO.Class
import Lib
import Parse
import CTL
import Eval
import Data.Set

main :: IO ()
main = do args <- getArgs
          contents <- readFile $ head args
          let model = func $ lexer contents
          let smodel = SMdl (ctlExpr model) (fromList (sts model)) (rels model) (vals model)
          let sts = eval smodel
          print smodel
          print sts
          
