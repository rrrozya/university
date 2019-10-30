module Main where

import           Axiom
import           Data.List       (foldl', intercalate)
import qualified Data.Map.Strict as M
import           Data.Maybe      (catMaybes)
import qualified Data.Set        as S
import           Expression
import           Glivenko
import           ParseStr
import           System.IO       (isEOF)

main :: IO ()
main = do
  (context, target) <- readHead
  readProof target context --(fitBody target context $!) <$> readBody

readHead :: IO ([Expression], Expression)
readHead = do
  head <- getLine
  let (contextStr, targetStr) = splitFirst head
  putStrLn $ contextStr ++ "|- !!(" ++ targetStr ++ ")"
  return (parseStr <$> splitWhen (== ',') contextStr, parseStr targetStr)

splitFirst :: String -> (String, String)
splitFirst ('|':'-':tail) = ([], tail)
splitFirst (c:tail) =
  let (h, t) = splitFirst tail
   in (c : h, t)

splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> w : splitWhen p s''
      where (w, s'') = break p s'

readProof :: Expression -> [Expression] -> IO ()
readProof target context = helper 1 (M.empty, M.empty, M.empty)
  where
    helper :: Int -> (M.Map Expression Int, M.Map Int Expression, M.Map Expression [Int]) -> IO ()
    helper ind maps@(e_i, i_e, implR) = do
      end <- isEOF
      if end
        then return ()
        else do
          line <- getLine
          if null line
            then helper ind maps
            else do
              let e = parseStr line
              processExpr e
              -- putStrLn ""
              if M.member e e_i
                then helper ind maps
                else helper (ind + 1) (M.insert e ind e_i, M.insert ind e i_e, update e)
      where
        processExpr :: Expression -> IO ()
        processExpr e
          | e `elem` context || isAnyAxiom e = genTruth e
          | isAxiom'10' e =
            let (_ :->: u) = e
             in genNot10 u
          | otherwise = genMP (getFrom e) e
        --
        update :: Expression -> M.Map Expression [Int]
        update (l :->: r) =
          case implR M.!? r of
            Nothing -> M.insert r [ind] implR
            Just l  -> M.insert r (ind : l) implR
        update _ = implR
        --
        getFrom :: Expression -> Expression
        getFrom e =
          head $
          catMaybes $
          (\(l :->: r) ->
             if M.member l e_i
               then Just l
               else Nothing) .
          (i_e M.!) <$>
          (implR M.! e)
