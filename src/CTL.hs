module CTL where

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
          | Parens CTL
          deriving Show

data List a = Nil | Cons a (List a) deriving Show 

-- State
type State = String
type States = List State

-- A transition between two states
type Relation = (State, State)
type Relations = List Relation

-- The states where an atomic is true
type Valuation = (Atomic, States)
type Valuations = List Valuation

-- The model itself
-- TODO: puede ser un data con campos?
-- type Model = (CTL, States, Relations, Valuations

data Model = Mdl {
    ctlExpr :: CTL,
    sts     :: States,
    rels    :: Relations,
    vals    :: Valuations 
}
