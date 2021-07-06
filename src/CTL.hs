module CTL where

-- We use a 'data' to represent a proposition
data CTL = Atomic String
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

-- State
type State = String

-- A transition between two states
type Transition = (State, State)

-- The label of the corresponding state
-- TODO: has to be CTL instead of [String]???
type Label = (State, [String])
-- don't know :(
type Model = ([State], CTL) 
