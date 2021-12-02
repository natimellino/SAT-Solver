{-# LANGUAGE PatternSynonyms #-}

module CTL where
import Data.Set

type Atomic = String

-- We use a 'data' to represent a logic proposition
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
          deriving Show

-- We use pattern synonyms for derivated operators

-- Para todo rombo
pattern AF :: CTL -> CTL
pattern AF ctl = AU Top ctl

-- Existe rombo
pattern EF :: CTL -> CTL
pattern EF ctl = EU Top ctl

-- Para todo cuadrado
pattern AG :: CTL -> CTL
pattern AG ctl = Not (EF (Not ctl))

-- Existe cuadrado
pattern EG :: CTL -> CTL
pattern EG ctl = Not (AF (Not ctl))

-- pattern THEN
-- completar con mas patterns 

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

