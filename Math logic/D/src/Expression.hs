module Expression where

infixl 7 :&:

infixl 6 :|:

infixr 5 :->:

data Expression
  = Expression :&: Expression
  | Expression :|: Expression
  | Expression :->: Expression
  | Not Expression
  | Var String
  deriving (Eq, Ord)

instance Show Expression where
  show (l :&: r)  = "(" ++ show l ++ " & " ++ show r ++ ")"
  show (l :|: r)  = "(" ++ show l ++ " | " ++ show r ++ ")"
  show (l :->: r) = "(" ++ show l ++ " -> " ++ show r ++ ")"
  show (Not e)    = "!" ++ show e
  show (Var var)  = var
