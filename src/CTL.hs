{-# LANGUAGE PatternSynonyms #-}

module CTL where
import Data.Set

type Atomic = String

-- We use a 'data' to represent a logic proposition
data CTL =  Atomic Atomic
          | Bottom
          | Not CTL
          | And CTL CTL
          | Or CTL CTL
          | AX CTL -- Para todo siguiente
          | EX CTL -- Existe siguiente
          | AU CTL CTL -- For All Until
          | EU CTL CTL -- Exists Until
          deriving Show

-- We use a 'data' to represent a logic proposition
data SCTL =  SAtomic Atomic
           | SBottom
           | SNot SCTL
           | SAnd SCTL SCTL
           | SOr SCTL SCTL
           | SAX SCTL -- Para todo siguiente
           | SEX SCTL -- Existe siguiente
           | SAU SCTL SCTL -- For All Until
           | SEU SCTL SCTL -- Exists Until
           | SAF SCTL
           | SEF SCTL
           | SAG SCTL
           | SEG SCTL
           | STHEN SCTL SCTL
           | STOP 
          deriving Show

-- We use pattern synonyms for derivated operators to simplify code.

-- -- Para todo rombo
-- pattern AF :: CTL -> CTL
-- pattern AF ctl = AU Top ctl

-- -- Existe rombo
-- pattern EF :: CTL -> CTL
-- pattern EF ctl = EU Top ctl

-- -- Para todo cuadrado
-- pattern AG :: CTL -> CTL
-- pattern AG ctl = Not (EF (Not ctl))

-- -- Existe cuadrado
-- pattern EG :: CTL -> CTL
-- pattern EG ctl = Not (AF (Not ctl))

-- pattern Then :: CTL -> CTL -> CTL
-- pattern Then p q = Or (Not p) q

-- pattern Top :: CTL
-- pattern Top = Not Bottom

-- State
type State = String

-- A transition between two states
type Relation = (State, State)

-- The states where an atomic is true
type Valuation = (Atomic, [State])

-- The model itself
data Model = Mdl {
    ctlExpr :: SCTL,
    sts     :: [State],
    rels    :: [Relation],
    vals    :: [Valuation] 
} deriving Show

-- We use this SModel similar to the Model data, to represents the states
-- sets instead of lists. This is because the parser can only parse directly
-- in to lists and we must do the conversion from list to set before evaluating.
data SModel = SMdl {
    sctlExpr :: CTL,
    sugarCtl :: SCTL, 
    ssts     :: Set State,
    srels    :: [Relation],
    svals    :: [Valuation] 
} deriving Show
