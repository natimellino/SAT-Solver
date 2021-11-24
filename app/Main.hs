module Main where

import System.Environment (getArgs)
import Control.Monad.IO.Class
import Lib
import Parse
import CTL
import Eval
import Data.Set
import Data.List
import PPrint

main :: IO ()
main = do args <- getArgs
          putStrLn $ ppPrompt "Reading file..."
          contents <- readFile $ head args
          let model = parseModel contents
          putStrLn $ ppPrompt "Evaluating..."
          let sts = eval model
          print model
          putStrLn ((ppResult "SAT: ") ++ (intersperse ' ' (concat $ toList sts)))


parseModel :: String -> SModel
parseModel contents = let model = func $ lexer contents
                          smodel = SMdl (ctlExpr model) (fromList (sts model)) (rels model) (vals model)
                      in smodel
          
