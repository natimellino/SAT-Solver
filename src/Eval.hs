module Eval where

import Monads
import CTL
import Data.List

-- let x = func $ lexer "STATES = [];RELATIONS = [];VALUATIONS = {};CTLEXP = AX p ;" 
-- in eval x

-- TODO: revisar esto tiene q tomar un ctl ???? hay algo raro
eval :: Model -> [State]
eval mdl = fst $ runState (sat (ctlExpr mdl)) mdl

sat :: MonadState m => CTL -> m [State]
sat Bottom = return []
sat Top = do sts <- getStates
             return sts
sat (Atomic str) = do vals <- getVals                   
                      return $ getMatchedStates str vals
sat (Not ctl) = do res <- sat ctl
                   sts <- getStates
                   return $ sts \\ res  
sat (And ctl ctl') = do res <- sat ctl
                        res' <- sat ctl'
                        return $ res `intersect` res'
sat (Or ctl ctl') = do res <- sat ctl
                       res' <- sat ctl'
                       return $ res `union` res'
sat (Then ctl ctl') = sat (Or (Not ctl) (ctl'))                                     
-- TODO: faltan los casos pikntes :(
sat _ = return []

-- Returns the states where and atomic is valid
getMatchedStates :: Atomic -> [Valuation] -> [State]
getMatchedStates _ [] = []
getMatchedStates at ((at', states):xs) = if at == at' then states
                                         else getMatchedStates at xs 