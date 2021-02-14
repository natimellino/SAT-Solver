module CTL where

data CTL = Atomic String
          | Bottom
          | Top
          | Not CTL
          | And CTL CTL
          | Or CTL CTL
          | Then CTL CTL
          | AX CTL -- Para todo siguiente
          | EX CTL -- Existe siguiente
          | AU CTL CTL-- For All Until
          | EU CTL CTL-- Exists Until
          | AF CTL -- Para todo rombo
          | EF CTL -- Existe Rombo
          | AG CTL -- Para todo cuadrado
          | EG CTL -- Existe cuadrado
          | Parens CTL
          deriving Show

type State = String
type Transition = (State, State)
type Label = (State, [String])
type Model = ([State], CTL) 
