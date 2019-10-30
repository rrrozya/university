module Main where

import           Data.List  (intercalate)
import           Expression
import           ParseStr
import           Proof

main :: IO ()
main = do
  targetStr <- getLine
  if null targetStr
    then putStrLn ":("
    else do
      let target = parseStr targetStr
      case processHyps target of
        Nothing     -> putStrLn ":("
        Just result -> outProof result

outProof :: (Expression, [Hypnotize]) -> IO ()
outProof (target, hyps) = do
  putStrLn $ intercalate ", " (hypToStr <$> hyps) ++ "|- " ++ show target
  putProof $ getProof target hyps
  where
    hypToStr :: Hypnotize -> String
    hypToStr (Yes var) = show (Var var)
    hypToStr (No var)  = show (Not (Var var))
    --
    putProof :: [Expression] -> IO ()
    putProof [] = return ()
    putProof (e:tail) = do
      print e
      putProof tail
