{ 
module Parse where

import CTL
import Data.Char
}

%name func 
%tokentype { Token } 
%error { parseError }

%token
    AT          { TAt $$ }
    BT          { TBT }
    TOP         { TTop }
    NOT         { TNot }
    '&'         { TAnd }
    '|'         { TOr }
    THEN        {TThen}
    EU          { TEu }
    AX          { TAx }
    EX          { TEx }
    A           { TAll }
    E           { TExists }
    U           { TUntil }
    AF          { TAf }
    EF          { TEf }
    AG          { TAg }
    EG          { TEg }
    '('         { TParenLeft}
    ')'         { TParenRight }
    '['         { TLBracket }
    ']'         { TRBracket }
    ','         { TComma }
    state       { TState $$ }

%%

-- TODO: considerar casos de listas vacias
--  arreglar el lexer de valuations q no anda el gil
-- hacer gramatica general para parsear todo :(
-- asociatividad y prioridad de las formulas ctl
-- no se

sts : '[' states ']'    { $2 }
     | '[' ']'          { Nil } 


states :: { List State} 
states : state                   { Cons $1 Nil }
       | state ',' states        { Cons $1 $3 }

vals : '[' valuations ']'    { $2 }

valuations :: { List Valuation }
valuations :  valuation                     { Cons $1 Nil }
            | valuation ',' valuations      { Cons $1 $3  }

valuation : '(' AT ',' sts ')'    { ($2, $4) }


rels : '[' relations ']'    { $2 }

relations :: { List Relation }
relations :   relation                  { Cons $1 Nil }
            | relation ',' relations    { Cons $1 $3 }

relation : '(' state ',' state ')'  { ($2, $4) } 


ctl :: { CTL }
ctl : AT                        { Atomic  $1 }
    | BT                        { Bottom }
    | TOP                       { Top }
    | NOT ctl                   { Not $2}
    | ctl '&' ctl               { And $1 $3 }
    | ctl '|' ctl               { Or $1 $3 }
    | ctl THEN ctl              { Then $1 $3 }
    | AX ctl                    { AX $2 }
    | EX ctl                    { EX $2 }
    | A '[' ctl  U ctl ']'      { AU $3 $5 }
    | EU ctl ctl                { EU $2 $3 }
    | AF ctl                    { AF $2 }
    | EF ctl                    { EF $2 }
    | AG ctl                    { AG $2 }
    | EG ctl                    { EG $2 }
    | '(' ctl ')'               { Parens $2 }


{
data Token = TAt String
            | TBT 
            | TTop
            | TNot
            | TAnd
            | TOr
            | TThen
            | TAx
            | TEx
            | TAll
            | TExists
            | TUntil
            | TEu
            | TAf
            | TEf
            | TAg
            | TEg
            | TParenLeft
            | TParenRight 
            | TLBracket
            | TRBracket
            | TComma
            | TState String

            deriving Show

parseError :: [Token] -> a
parseError _ = error "Parse error"

-- TODO: fijarse de hacer otro lexer auxiliar y ver donde llamarlo

-- Propositions

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
lexer ('&':cs) = TAnd : lexer cs
lexer ('|':cs) = TOr : lexer cs
lexer ('(':cs) = TParenLeft : lexer cs
lexer (')':cs) = TParenRight : lexer cs
lexer ('[':cs) = TLBracket : lexer cs
lexer (']':cs) = TRBracket : lexer cs

lexVar :: String -> [Token]
lexVar cs = case span isAlpha cs of
                ("BT", rest) -> TBT : lexer rest
                ("TOP", rest) -> TTop : lexer rest
                ("NOT", rest) -> TNot : lexer rest
                ("THEN", rest) -> TThen : lexer rest
                ("AX", rest) -> TAx : lexer rest
                ("EX", rest) -> TEx : lexer rest
                ("AF", rest) -> TAf : lexer rest
                ("EF", rest) -> TEf : lexer rest
                ("AG", rest) -> TAg : lexer rest
                ("EG", rest) -> TEg : lexer rest
                ("EU", rest) -> TEu : lexer rest
                (var, rest) -> (TAt var) : lexer rest

-- States

lexer4states :: String -> [Token]
lexer4states [] = []
lexer4states (',':cs) = TComma : lexer4states cs
lexer4states ('[':cs) = TLBracket : lexer4states cs
lexer4states (']':cs) = TRBracket : lexer4states cs
lexer4states (c:cs)
             | isSpace c = lexer4states cs
             | isAlpha c = lexState (c:cs)

lexState :: String -> [Token]
lexState [] = []
lexState cs = case span isAlpha cs of
                (var, rest) -> (TState var) : lexer4states rest

-- Relations

lexer4relations :: String -> [Token]
lexer4relations [] = []
lexer4relations ('[':cs) = TLBracket : lexer4relations cs
lexer4relations (']':cs) = TRBracket : lexer4relations cs
lexer4relations (',':cs) = TComma : lexer4relations cs
lexer4relations ('(':cs) = TParenLeft : lexRelation cs
lexer4relations (c:cs) | isSpace c = lexer4relations cs


lexRelation :: String -> [Token]
lexRelation [] = []
lexRelation (')':cs) = TParenRight : lexer4relations cs
lexRelation (',':cs) = TComma : lexRelation cs
lexRelation css@(c:cs)
                | isSpace c = lexRelation cs
                | isAlpha c = case span isAlpha css of
                                (var, rest) -> (TState var) : lexRelation rest

-- Valuations

lexer4valuations :: String -> [Token]
lexer4valuations [] = []
lexer4valuations ('[':cs) = TLBracket : lexer4valuations cs
lexer4valuations (']':cs) = TRBracket : lexer4valuations cs
lexer4valuations (',':cs) = TComma : lexer4valuations cs
lexer4valuations ('(': cs) = TParenLeft : lexValuation cs
lexer4valuations (c:cs) | isSpace c = lexer4valuations cs

lexValuation :: String -> [Token]
lexValuation [] = []
lexValuation (')':cs) = TParenRight : lexer4valuations cs
lexValuation (',':cs) = TComma : lexValuation cs
lexValuation ('[':cs) = valuationStates cs
lexValuation css@(c:cs)
                    | isSpace c = lexValuation cs
                    | isAlpha c = case span isAlpha css of
                        (var, rest) -> (TAt var) : lexValuation rest 

-- FIXME: codigo repetidooooooooooooooooooooooo aaaaaaaaaaaaaaaaaaaaaa

valuationStates :: String -> [Token]
valuationStates [] = []
valuationStates (',':cs) = TComma : valuationStates cs
valuationStates (']':cs) = TRBracket : lexValuation cs
valuationStates (c:cs)
             | isSpace c = valuationStates cs
             | isAlpha c = valuationState (c:cs)

valuationState :: String -> [Token]
valuationState [] = []
valuationState cs = case span isAlpha cs of
                        (var, rest) -> (TState var) : valuationStates rest


}  