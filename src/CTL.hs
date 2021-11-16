module CTL where

import Data.Set

type Atomic = String

-- We use a 'data' to represent a proposition
data CTL = Atomic Atomic
          | Bottom
          | Top
          | Not CTL
          | And CTL CTL
          | Or CTL CTL
          | Then CTL CTL
          | AX CTL -- Para todo siguiente
          | EX CTL -- Existe siguiente
          | AU CTL CTL -- For All Until
          | EU CTL CTL -- Exists Until
          | AF CTL -- Para todo rombo
          | EF CTL -- Existe Rombo
          | AG CTL -- Para todo cuadrado
          | EG CTL -- Existe cuadrado
          deriving Show

-- State
type State = String

-- A transition between two states
type Relation = (State, State)

-- The states where an atomic is true
type Valuation = (Atomic, [State])

-- The model itself
data Model = Mdl {
    ctlExpr :: CTL,
    sts     :: [State],
    rels    :: [Relation],
    vals    :: [Valuation] 
} deriving Show

data SModel = SMdl {
    sctlExpr :: CTL,
    ssts     :: Set State,
    srels    :: [Relation],
    svals    :: [Valuation] 
} deriving Show


data Error = Err1 | Err2 deriving (Eq, Show)

