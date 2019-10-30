module Main where

import           Expression
import           Lexer      (alexScanTokens)
import           Parser     (parseExpr)

main :: IO ()
main = do
  input <- getContents
  case parseExpr (alexScanTokens input) of
    Left err   -> putStrLn err
    Right expr -> putStrLn $ show expr
