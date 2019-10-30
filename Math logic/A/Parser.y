{
module Parser where

import Expression
import Lexer
}

%name      parseExpr
%tokentype { Token }
%error     { parseError }
%monad     { Either String }{ >>= }{ return }

%token IDENT   { Ident $$ }
%token LEFTP   { LeftP }
%token RIGHTP  { RightP }
%token NOT     { NotT }
%token AND     { AndT }
%token OR      { OrT }
%token IMPL    { ImplT }

%%

Expression
  : Disj                      			{ $1 }
  | Disj IMPL Expression                { Binary Impl $1 $3 }

Disj
  : Conj                                { $1 }
  | Disj OR Conj                        { Binary Or $1 $3}

Conj
  : Neg                                 { $1 }
  | Conj AND Neg                        { Binary And $1 $3}

Neg
  : NOT Neg                             { Unary Negate $2 }
  | Var                                 { $1 }
  | LEFTP Expression RIGHTP             { $2 }

Var
  : IDENT                               { Var $1 }

{
parseError = fail "Parse error"
}
