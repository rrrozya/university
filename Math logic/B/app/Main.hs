module Main where

import           Data.List       (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           Expression
import           ParseStr
import           Proof
import           System.IO       (isEOF)

main :: IO ()
main = do
  (context, target) <- readHead
  result <- readProof target context --(fitBody target context $!) <$> readBody
  case result of
    Nothing -> putStrLn "Proof is incorrect"
    Just ((e_i, i_e, _), anLst) -> do
      putStrLn $! outHead context target
      minimizeProof (e_i M.! target) i_e anLst

readHead :: IO ([Expression], Expression)
readHead = do
  (contextStr, targetStr) <- splitFirst <$> getLine
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

{-readBody :: IO [Expression]
readBody = do
  end <- isEOF
  if end
    then return []
    else do
      line <- getLine
      if null line
        then readBody
        else ((parseStr line :) $!) <$> readBody-}

readProof :: Expression -> [Expression] -> IO (Maybe (Maps, [Annotated]))
readProof target context = helper 1 (M.empty, M.empty, M.empty) False
  where
    helper :: Int -> Maps -> Bool -> IO (Maybe (Maps, [Annotated]))
    helper ind maps@(e_i, i_e, implR) isLastTarget = do
      end <- isEOF
      if end
        then return
               (if isLastTarget
                  then Just (maps, [])
                  else Nothing)
        else do
          line <- getLine
          if null line
            then helper ind maps isLastTarget
            else do
              let e = parseStr line
              if M.member e e_i
                then helper ind maps (e == target)
                else case checkProof context target maps e of
                       Nothing -> return Nothing
                       Just an -> do
                         mbResult <- helper (ind + 1) (M.insert e ind e_i, M.insert ind e i_e, update e) (e == target)
                         case mbResult of
                           Nothing       -> return Nothing
                           Just (m, lst) -> return $ Just (m, an : lst)
      where
        update :: Expression -> M.Map Expression [Int]
        update (l :->: r) =
          case implR M.!? r of
            Nothing -> M.insert r [ind] implR
            Just l  -> M.insert r (ind : l) implR
        update _ = implR

{-fitBody :: Expression -> [Expression] -> [Expression] -> Maybe (Maps, [Annotated])
fitBody target context = helper 1 (M.empty, M.empty, M.empty)
  where
    helper :: Int -> Maps -> [Expression] -> Maybe (Maps, [Annotated])
    helper _ _ [] = Nothing
    helper ind maps@(e_i, i_e, implR) [a]
      | a /= target = Nothing
      | M.member a e_i = Just (maps, [])
      | otherwise = do
        an <- checkProof context target maps a
        return ((M.insert a ind e_i, M.insert ind a i_e, M.empty), [an])
    helper ind maps@(e_i, i_e, implR) (e:tail) =
      if M.member e e_i
        then helper ind maps tail
        else do
          an <- checkProof context target maps e
          (ms, lst) <- helper (ind + 1) (M.insert e ind e_i, M.insert ind e i_e, update e) tail
          return (ms, an : lst)
      where
        update :: Expression -> M.Map Expression [Int]
        update (l :->: r) =
          case implR M.!? r of
            Nothing -> M.insert r [ind] implR
            Just l  -> M.insert r (ind : l) implR
        update _ = implR-}

{-outProof :: Int -> M.Map Int Expression -> [(Int, Annotated)] -> IO ()
outProof _ _ [] = return ()
outProof ind mapper ((i, an):tail) = do
  putStrLn $!
    case an of
      Axiom n -> "[" ++ show ind ++ ". Ax. sch. " ++ show n ++ "] " ++ show (mapper M.! i)
      Context n -> "[" ++ show ind ++ ". Hypothesis " ++ show n ++ "] " ++ show (mapper M.! i)
      MP r l -> "[" ++ show ind ++ ". M.P. " ++ show l ++ ", " ++ show r ++ "] " ++ show (mapper M.! i)
  outProof (ind + 1) mapper tail-}
outHead :: [Expression] -> Expression -> String
outHead [] tgt = "|- " ++ show tgt
outHead (h:ctx) tgt = foldl' (\acc e -> acc ++ ", " ++ show e) (show h) ctx ++ " |- " ++ show tgt
