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
    AU          { TAu }
    AX          { TAx }
    EX          { TEx }
    E           { TExists }
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
    ';'         { TSemicolon }
    '='         { TEqual }
    state       { TState $$ }
    States      { TStates }
    Relations   { TRelations }
    Valuations  { TValuations }
    CTLExp      { TCTLExp }

%left '&' '|' 
%left THEN EU AU
%nonassoc NOT AF EF AG EG AX AE

%%

-- asociatividad y prioridad de las formulas ctl
-- es realmente necesario los reverse???
-- no se

fields :: { Model }
fields :   States     '=' sts   ';'       
           Relations  '=' rels  ';'     
           Valuations '=' vals  ';'     
           CTLExp     '=' ctl   ';'   {Mdl $15 (reverse $3) (reverse $7) (reverse $11) }        

vals :    '{' valuations '}'       { $2 }
       |  '{' '}'                  { [] }

valuations :: { [Valuation] }
valuations :   valuation                     { [$1] }
             | valuations ',' valuation      { $3 : $1 }

valuation :: { Valuation }
valuation :  AT ':' sts     { ($1, $3) }


sts :  '[' states ']'    { $2 }
     | '[' ']'           { [] } 


states :: { [State] } 
states : state                   { [$1] }
       | states ',' state        { $3 : $1 }

rels :    '[' relations ']'    { $2 }
        | '[' ']'              { [] }

relations :: { [Relation] }
relations :   relation                  { [$1] }
            | relations ',' relation    { $3 : $1 }

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
    | AU ctl ctl                { AU $2 $3 }
    | EU ctl ctl                { EU $2 $3 }
    | AF ctl                    { AF $2 }
    | EF ctl                    { EF $2 }
    | AG ctl                    { AG $2 }
    | EG ctl                    { EG $2 }
    | '(' ctl ')'               { $2 }


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
            -- | TAll
            | TExists
            -- | TUntil
            | TEu
            | TAu
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
            | TSemicolon
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
lexer cs@(c:cc) | "STATES" `isPrefixOf` cs = TStates : lexer4states (drop 6 cs)
                | "RELATIONS" `isPrefixOf` cs =  TRelations : lexer4relations (drop 9 cs) 
                | "VALUATIONS" `isPrefixOf` cs = TValuations : lexer4valuations (drop 10 cs)  
                | "CTLEXP" `isPrefixOf` cs = TCTLExp : lexerCTL (drop 6 cs)
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
lexerCTL ('=':cs) = TEqual : lexerCTL cs
lexerCTL (';':cs) = TSemicolon : lexer cs

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
                ("AU", rest) -> TAu : lexerCTL rest
                (var, rest) -> (TAt var) : lexerCTL rest

-- States

lexer4states :: String -> [Token]
lexer4states [] = []
lexer4states (',':cs) = TComma : lexer4states cs
lexer4states ('[':cs) = TLBracket : lexer4states cs
lexer4states (']':cs) = TRBracket : lexer4states cs
lexer4states (';':cs) = TSemicolon : lexer cs
lexer4states ('=':cs) = TEqual : lexer4states cs
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
lexer4relations (';':cs) = TSemicolon : lexer cs
lexer4relations ('=':cs) = TEqual : lexer4relations cs
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
lexer4valuations ('{':cs) = TLeftBrace : lexer4valuations cs
lexer4valuations ('}':cs) = TRightBrace : lexer4valuations cs
lexer4valuations (',':cs) = TComma : lexer4valuations cs
lexer4valuations (';':cs) = TSemicolon : lexer cs
lexer4valuations ('=':cs) = TEqual : lexer4valuations cs
lexer4valuations css@(c:cs) | isSpace c = lexer4valuations cs
                            | isAlpha c = lexValuation css

lexValuation :: String -> [Token]
lexValuation [] = []
lexValuation (':':cs) = TDDot : valuationStates cs
lexValuation css@(c:cs) | isSpace c = lexValuation cs
                        | isAlpha c = case span isAlpha css of
                                        (var, rest) -> (TAt var) : lexValuation rest

valuationStates :: String -> [Token]
valuationStates [] = []
valuationStates ('[':cs) = TLBracket : valuationStates cs
valuationStates (',':cs) = TComma : valuationStates cs
valuationStates (']':cs) = TRBracket : lexer4valuations cs
valuationStates css@(c:cs) | isSpace c = valuationStates cs
                           | isAlpha c = case span isAlpha css of 
                                            (var, rest) -> (TState var) : valuationStates rest

}