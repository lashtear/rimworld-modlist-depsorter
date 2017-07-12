{
module RuleParser (parse) where

import RuleTypes

}

%name parse
%error { parseError }
%tokentype { Token }

%token
    has { TokenHas }
    not { TokenNot }
    and { TokenAnd }
    or { TokenOr }
    starting { TokenStart }
    containing { TokenContain }
    text { TokenText $$ }
    hard { TokenHard }
    soft { TokenSoft }
    '.' { TokenDot }
    ',' { TokenComma }
    '(' { TokenOP }
    ')' { TokenCP }

%right starting
%right containing
%nonassoc not
%left and
%left or
%right ','

%%

Rules ::			{ [Rule] }
Rules : Rule			{ [$1] }
    | Rules Rule		{ $2 : $1 }

Rule ::			{ Rule }
Rule : Exp has Dep Exp '.'	{ Rule $1 $3 $4 }

Exp ::				{ Exp }
Exp : text			{ ExpString $1 }
    | bp(Exp)			{ $1 }
    | starting Exp		{ expStart $2 }
    | containing  Exp		{ expContain $2 }
    | not Exp			{ ExpNot $2 }
    | bp(list3c(Exp,',',and))	{ ExpAnd $1 }
    | bp(list3c(Exp,',',or))	{ ExpOr $1 }
    | Exp and Exp		{ ExpAnd [$1, $3] }
    | Exp or Exp		{ ExpOr [$1, $3] }

opt(p) : p			{ Just $1 }
    |				{ Nothing }

rlist2(p,s) : p s p		{ [$3, $1] }
    | rlist2(p,s) s p		{ $3 : $1 }

rlist3c(p,s,c) : rlist2(p,s) opt(s) c p { $4 : $1 }

list3c(p,s,c) : rlist3c(p,s,c)	{ reverse $1 }

bp(p) : '(' p ')'		{ $2 }

Dep ::				{ Dep }
Dep : hard			{ Hard }
    | soft			{ Soft }

{
parseError :: [Token] -> a
parseError ts = error $ "parse error before " ++ (show ts)

}