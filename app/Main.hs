module Main where

import System.Environment (getArgs)
import Control.Monad.IO.Class
import Parse
import CTL
import Eval
import Data.Set
import Data.List
import PPrint
import Sugar

main :: IO ()
main = do args <- getArgs
          putStrLn $ ppPrompt "Reading file..."
          let filename = head args
          if ".sat" `isSuffixOf` filename then
            do contents <- readFile filename
               let smodel = parseModel contents
               putStrLn $ ppPrompt "Verifying model ..."
               let (verified, errorMsg) = verifyModel smodel
               if verified then
                 do putStrLn $ ppPrompt "Evaluating..."
                    let sts = eval smodel
                    printCTL (sugarCtl smodel)
                    printResult sts
               else
                 do putStrLn $ ppError "SOME ERRORS WERE ENCOUNTERED WHILE VERIFYING THE MODEL:" 
                    putStrLn $ ppError errorMsg
          else 
            do putStrLn $ ppError "ERROR: The file extension is not valid, make sure you are giving a .sat file"

-- Parses the content of the input file and converts the model to an smodel
-- using sets to represent the states instead of lists.
parseModel :: String -> SModel
parseModel contents = let model = func $ lexer contents
                          ctl = desugar (ctlExpr model) 
                          smodel = SMdl ctl (ctlExpr model) (fromList (sts model)) (rels model) (vals model)
                      in smodel

-- These are some dummy functions used to print the output

printCTL :: SCTL -> IO ()
printCTL ctl = putStrLn (ppResult  "SAT FOR " ++ (ppFormula ctl) ++ (ppResult ":"))

printResult :: Set State -> IO ()
printResult sts = go (toList sts)
                  where go [] = putStrLn uemptyset
                        go xs = let ys = Data.List.map (\s -> s ++ " ")xs
                                in putStrLn (concat ys) 

-- Auxiliar functions to do some verifications before the model is evaluated.

verifyModel :: SModel -> (Bool, String)
verifyModel smodel = let (isOkRels, msg1) = verifyRels (srels smodel) (toList (ssts smodel))
                         (isOkVals, msg2) = verifyVals (svals smodel) (toList (ssts smodel))
                     in (isOkRels && isOkVals, msg1 ++ msg2)

-- We must verify that every state given in a relation (s1, s2) exists.
verifyRels :: [Relation] -> [State] -> (Bool, String)
verifyRels [] _ = (True, "")
verifyRels (r@(s1, s2): xs) sts = if (s1 `elem` sts) && (s2 `elem` sts) then
                                  verifyRels xs sts
                                else 
                                  (False, "The relation " ++ (show r) ++ " is not valid. \n")

-- We must verify that all the states given for an atomic formula exists in the
-- given list of states.
verifyVals :: [Valuation] -> [State] -> (Bool, String)
verifyVals [] _  = (True, "")
verifyVals ((a, xs): xss) sts =  let boolList = Data.List.map (\x -> x `elem` sts) xs
                                     areAllStatesValid = and boolList
                             in if areAllStatesValid then
                                  verifyVals xss sts
                                else (False, "Some of the states are not valid for the atomic proposition \"" ++ a ++ "\"")
