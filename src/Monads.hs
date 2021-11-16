module Monads where

import CTL
import Control.Monad ( liftM, ap )
import Data.Set              

class Monad m => MonadState m where
    getStates :: m (Set State)
    getCTL    :: m CTL
    getRels   :: m [Relation]
    getVals   :: m [Valuation]

class Monad m => MonadError m where
    throw :: Error -> m a

-- Mónada de estado

newtype St a = State { runState :: SModel -> (a, SModel) }

-- Para calmar al GHC
instance Functor St where
  fmap = liftM

instance Applicative St where
  pure  = return
  (<*>) = ap

instance Monad St where
    return x = State (\s -> (x, s))
    m >>= f = State (\s -> let (v, s') = runState m s in runState (f v) s')

instance MonadState St where
    getStates = State (\s -> (ssts s, s))
    getCTL    = State (\s -> (sctlExpr s, s)) 
    getRels   = State (\s -> (srels s, s)) 
    getVals   = State (\s -> (svals s, s)) 

