module Monads where

import CTL
import Control.Monad ( liftM, ap )              

class Monad m => MonadState m where
    -- Cambia el valor de la proposición lógica
    updateProp :: CTL -> m () 
    -- Obtiene la proposición lógica
    getProp :: m CTL

class Monad m => MonadError m where
    throw :: Error -> m a

-- Mónada de estado

newtype St a = State { runState :: Model -> (a, Model) }

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
    updateProp ctl = return ()
    getProp = return (Not (Atomic "p")) 

