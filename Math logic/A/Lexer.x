{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [A-Z]
$ap = '

tokens :-

  $white+                    	 ;
  "#".*                      	 ;
  \(                         	 { \_ -> LeftP }
  \)                         	 { \_ -> RightP }
  \|                         	 { \_ -> OrT }
  &                         	 { \_ -> AndT }
  \-\>                         	 { \_ -> ImplT }
  !                         	 { \_ -> NotT }
  $alpha [$alpha $digit $ap]*    { \s -> Ident s }

{

data Token = LyambdaT
           | OrT
           | AndT
           | ImplT
           | NotT
           | LeftP
           | RightP
           | Ident String
           deriving (Show, Eq)

}
