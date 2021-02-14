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

states :: { [State] } 
states : state                   { [$1] }
       | state ',' states        { $1 : $3 }

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
    | A '[' ctl  U ctl ']'      {AU $3 $5}
    | EU ctl ctl                {EU $2 $3}
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
}  