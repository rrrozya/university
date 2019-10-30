module ParseStr where

import           Expression
import           Lexer
import           Parser

parseStr :: String -> Expression
parseStr s =
  case parseExpr (alexScanTokens s) of
    Left _    -> error "Proof is incorrect"
    Right exp -> exp
