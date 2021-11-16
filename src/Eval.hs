module Eval where

import Monads
import CTL
import Data.List

-- let x = func $ lexer "STATES = [];RELATIONS = [];VALUATIONS = {};CTLEXP = AX p ;" 
-- in eval x

-- TODO: dejar de usar listas y ver de usar conjuntos si es mas eficiente

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
sat (EX ctl) = do res <- sat ctl
                  preExists res
sat (AX ctl) = do res <- sat ctl
                  preForAll res                                     
-- TODO: faltan los casos pikntes :(
sat _ = return []

-- Returns the states where an atomic is valid
getMatchedStates :: Atomic -> [Valuation] -> [State]
getMatchedStates _ [] = []
getMatchedStates at ((at', states):xs) = if at == at' then states
                                         else getMatchedStates at xs


-- TODO: testear estas funcionessss
preExists :: MonadState m => [State] -> m [State]
preExists [] = return []
preExists (xs) = do sts <- getStates
                    rels <- getRels
                    return $ go sts xs rels
                where go [] _  _= []
                      go (s:sts) xs rels = let l = filter (\s'-> (s, s') `elem` rels ) xs
                                               l' = go sts xs rels
                                           in if length l > 0 then s:l'
                                              else l'  -- Eliminar elementos repetidos

preForAll :: MonadState m => [State] -> m [State]
preForAll [] = return []
preForAll (xs) = do sts <- getStates
                    rels <- getRels
                    return $ go sts xs rels
                where go [] _  _= []
                      go (s:sts) xs rels = let l = filter (\s'-> (s, s') `elem` rels ) xs
                                               l' = go sts xs rels
                                           in if length l  == length xs then s:l'
                                              else l'  -- Eliminar elementos repetidos

inev = undefined

existsUntil = undefined

forAllUntil = undefined