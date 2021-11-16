module Eval where

import Monads
import CTL

-- let x = func $ lexer "STATES = [];RELATIONS = [];VALUATIONS = {};CTLEXP = AX p ;" 
-- in eval x


-- TODO: revisar esto tiene q tomar un ctl ???? hay algo raro
-- eval :: CTL -> [State]
-- eval mdl = fst $ runState (eval' mdl) mdl

-- eval' :: MonadState m => Model -> m [State]
-- eval' mdl = return []