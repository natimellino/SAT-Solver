{ 
module Parse where

import CTL
import Data.Char
import Data.List
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
    '{'         { TLeftBrace }
    '}'         { TRightBrace }
    ':'         { TDDot }
    '='         { TEqual }
    state       { TState $$ }
    States      { TStates }
    Relations   { TRelations }
    Valuations  { TValuations }
    CTLExp      { TCTLExp }

%%

-- TODO: considerar casos de listas vacias
--  arreglar el lexerCTL de valuations q no anda el gil
-- hacer gramatica general para parsear todo :(
-- asociatividad y prioridad de las formulas ctl
-- no se

fields :: { Model }
fields :   States     '=' sts        
           Relations  '=' rels       
           Valuations '=' vals       
           CTLExp     '=' ctl       {Mdl $12 $3 $6 $9 }        

vals :    '{' valuations '}'       { $2 }
       |  '{' '}'                  { Nil }

valuations :: { List Valuation }
valuations :   valuation                     { Cons $1 Nil }
             | valuation ',' valuations      { Cons $1 $3 }

valuation :: { Valuation }
valuation :  AT ':' sts     { ($1, $3) }


sts :  '[' states ']'    { $2 }
     | '[' ']'           { Nil } 


states :: { List State} 
states : state                   { Cons $1 Nil }
       | state ',' states        { Cons $1 $3 }

rels :    '[' relations ']'    { $2 }
        | '[' ']'              { Nil }

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
data Token =  TAt String
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
            | TRightBrace
            | TLeftBrace
            | TDDot
            | TEqual
            | TStates
            | TRelations
            | TValuations
            | TCTLExp

            deriving Show

parseError :: [Token] -> a
parseError _ = error "Parse error"

-- Main lexer
lexer :: String -> [Token]
lexer [] = []
lexer cs@(c:cc) | "STATES" `isPrefixOf` cs = []
                | "RELATIONS" `isPrefixOf` cs = [] 
                | "VALUATIONS" `isPrefixOf` cs = []  
                | "CTLEXP" `isPrefixOf` cs = []
                | isSpace c = lexer cc

-- Propositions

lexerCTL :: String -> [Token]
lexerCTL [] = []
lexerCTL (c:cs) | isSpace c = lexerCTL cs
                | isAlpha c = lexVar (c:cs)
lexerCTL ('&':cs) = TAnd : lexerCTL cs
lexerCTL ('|':cs) = TOr : lexerCTL cs
lexerCTL ('(':cs) = TParenLeft : lexerCTL cs
lexerCTL (')':cs) = TParenRight : lexerCTL cs
lexerCTL ('[':cs) = TLBracket : lexerCTL cs
lexerCTL (']':cs) = TRBracket : lexerCTL cs

lexVar :: String -> [Token]
lexVar cs = case span isAlpha cs of 
                ("BT", rest) -> TBT : lexerCTL rest
                ("TOP", rest) -> TTop : lexerCTL rest
                ("NOT", rest) -> TNot : lexerCTL rest
                ("THEN", rest) -> TThen : lexerCTL rest
                ("AX", rest) -> TAx : lexerCTL rest
                ("EX", rest) -> TEx : lexerCTL rest
                ("AF", rest) -> TAf : lexerCTL rest
                ("EF", rest) -> TEf : lexerCTL rest
                ("AG", rest) -> TAg : lexerCTL rest
                ("EG", rest) -> TEg : lexerCTL rest
                ("EU", rest) -> TEu : lexerCTL rest
                (var, rest) -> (TAt var) : lexerCTL rest

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
                | isAlpha c = case span isAlpha cs of 
                                (var, rest) -> (TState var) : lexRelation rest

-- Valuations

lexer4valuations :: String -> [Token]
lexer4valuations [] = []
lexer4valuations ('{':cs) = TLeftBrace : lexer4valuations cs
lexer4valuations ('}':cs) = TRightBrace : lexer4valuations cs
lexer4valuations (',':cs) = TComma : lexer4valuations cs
lexer4valuations css@(c:cs) | isSpace c = lexer4valuations cs
                            | isAlpha c = lexValuation css

lexValuation :: String -> [Token]
lexValuation [] = []
lexValuation (':':cs) = TDDot : valuationStates cs
lexValuation css@(c:cs) | isSpace c = lexValuation cs
                        | isAlpha c = case span isAlpha cs of
                                        (var, rest) -> (TAt var) : lexValuation rest

valuationStates :: String -> [Token]
valuationStates [] = []
valuationStates ('[':cs) = TLBracket : valuationStates cs
valuationStates (',':cs) = TComma : valuationStates cs
valuationStates (']':cs) = TRBracket : lexer4valuations cs
valuationStates css@(c:cs) | isSpace c = valuationStates cs
                           | isAlpha c = case span isAlpha cs of 
                                            (var, rest) -> (TState var) : valuationStates rest

}