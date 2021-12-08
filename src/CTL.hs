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

-- This data is the same as the previous one but it contains
-- sintactic sugar for some terms.
data SCTL =  SAtomic Atomic
           | SBottom
           | SNot SCTL
           | SAnd SCTL SCTL
           | SOr SCTL SCTL
           | SANext SCTL -- Para todo siguiente
           | SENext SCTL -- Existe siguiente
           | SAU SCTL SCTL -- For All Until
           | SEU SCTL SCTL -- Exists Until
           | SAF SCTL
           | SEF SCTL
           | SAG SCTL
           | SEG SCTL
           | SThen SCTL SCTL
           | Top 
          deriving Show

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
    sugarCtl :: SCTL, -- We store the original ctl expr exactly as it was 
                      -- parsed so we can pretty print correctly
    ssts     :: Set State,
    srels    :: [Relation],
    svals    :: [Valuation] 
} deriving Show
