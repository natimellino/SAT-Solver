module Eval where

import Monads
import CTL
-- import Data.List
import Data.Set

-- let x = func $ lexer "STATES = [];RELATIONS = [];VALUATIONS = {};CTLEXP = AX p ;" 
-- in eval x

eval :: SModel -> Set State
eval mdl = fst $ runState (sat (sctlExpr mdl)) mdl

sat :: MonadState m => CTL -> m (Set State)
sat Bottom = return empty
sat (Atomic str) = do vals <- getVals                   
                      return $ getMatchedStates str vals
sat (Not ctl) = do res <- sat ctl
                   sts <- getStates
                   return $ sts \\ res  
sat (And ctl ctl') = do res <- sat ctl
                        res' <- sat ctl'
                        return $ res `intersection` res'
sat (Or ctl ctl') = do res <- sat ctl
                       res' <- sat ctl'
                       return $ res `union` res'
sat (EX ctl) = do res <- sat ctl
                  preExists res
sat (AX ctl) = do res <- sat ctl
                  preForAll res                                     
sat (EU ctl ctl') = do res <- sat ctl
                       res' <- sat ctl'
                       existsUntil res res'
sat (AU ctl ctl') = do res <- sat ctl
                       res' <- sat ctl'
                       forAllUntil res res'                    

-- Returns the states where an atomic is valid
getMatchedStates :: Atomic -> [Valuation] -> Set State
getMatchedStates _ [] = empty
getMatchedStates at ((at', states):xs) = if at == at' then fromList states
                                         else getMatchedStates at xs


preExists :: MonadState m => Set State -> m (Set State)
preExists xs = do sts <- getStates
                  rels <- getRels
                  let pre = go (toList sts) (toList xs) rels 
                  return $ fromList pre
              where go [] _  _= []
                    go (s:sts) xs rels = let l' = go sts xs rels
                                         in if any (\s'-> (s, s') `elem` rels ) xs then s:l'
                                            else l'  

-- Usamos la igualdad: preForAll(Y) = S - preExists(S - Y)
preForAll :: MonadState m => Set State -> m (Set State)
preForAll xs = do sts <- getStates
                  pre <- preExists (sts \\ xs)
                  return $ sts \\ pre

inev :: MonadState m => Set State -> m (Set State)
inev xs = do xs' <- preForAll xs  
             let ys = xs `union` xs'
             if xs /= ys then inev ys 
             else return xs

existsUntil ::  MonadState m => Set State -> Set State -> m (Set State)
existsUntil res1 res2 = do preE <- preExists res2
                           let res = res2 `union` (res1 `intersection` preE)
                           if res2 /= res then existsUntil res1 res
                           else return res2

forAllUntil ::  MonadState m => Set State -> Set State -> m (Set State)
forAllUntil res1 res2 = do preE <- preForAll res2
                           let res = res2 `union` (res1 `intersection` preE)
                           if res2 /= res then forAllUntil res1 res
                           else return res2