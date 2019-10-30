module Expression where

data Expression = Var String | Unary UnOp Expression | Binary BinOp Expression Expression
    deriving (Eq)

data UnOp = Negate
    deriving (Eq)

data BinOp = Impl | Or | And
    deriving (Eq)

instance Show UnOp where
    show Negate = "!"

instance Show BinOp where
    show Impl = "->"
    show Or   = "|"
    show And  = "&"

instance Show Expression where
    show (Var var)                 = var
    show (Unary unOp expr)         = "(" ++ show unOp ++ show expr ++ ")"
    show (Binary binOp left right) = "(" ++ show binOp ++ "," ++ show left ++ "," ++ show right ++ ")"
-- example :: Expression
-- example = Ab "a" (Ab "b" (Ap (Ap (Ap (Ap (Var "a") (Var "b"))(Var "c")) (Ab "d" (Ap (Var "e") (Ab "f" (Var "g")))))(Var "h")))
