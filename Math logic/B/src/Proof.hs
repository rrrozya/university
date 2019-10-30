module Proof where

import           Axiom
import           Data.List       (elemIndex)
import qualified Data.Map.Strict as M
import           Data.Maybe      (catMaybes)
import qualified Data.Set        as S
import           Expression

data Annotated
  = Axiom Int
  | Context Int
  | MP Int Int
  deriving (Show)

type Maps = (M.Map Expression Int, M.Map Int Expression, M.Map Expression [Int])

checkProof :: [Expression] -> Expression -> Maps -> Expression -> Maybe Annotated
checkProof context target (e_i, i_e, implR) e = mbContext e ?: mbAxiom e ?: mbMP e
  where
    mbContext :: Expression -> Maybe Annotated
    mbContext e = do
      ind <- elemIndex e context
      return $! Context (ind + 1)
    --
    mbAxiom :: Expression -> Maybe Annotated
    mbAxiom e = do
      ind <- whichAxiom e
      return $! Axiom ind
    --
    mbMP :: Expression -> Maybe Annotated
    mbMP e = do
      lst <- implR M.!? e
      (l, r) <- getFst $ catMaybes $ tryMP <$> lst
      return $! MP l r
      where
        getFst :: [a] -> Maybe a
        getFst []    = Nothing
        getFst (a:_) = Just a
        --
        tryMP :: Int -> Maybe (Int, Int)
        tryMP i = do
          (l :->: r) <- i_e M.!? i
          lInd <- e_i M.!? l
          return (lInd, i)

infixr 0 ?:

(?:) :: Maybe a -> Maybe a -> Maybe a
jst@(Just _) ?: _ = jst
Nothing ?: mby = mby

minimizeProof :: Int -> M.Map Int Expression -> [Annotated] -> IO ()
minimizeProof target mapperE body = toList $ mark target
  where
    numList = zip [1,2 ..] body
    --
    mapperA = M.fromAscList numList
    --
    mark :: Int -> S.Set Int
    mark ind =
      case mapperA M.! ind of
        Axiom {}   -> S.singleton ind
        Context {} -> S.singleton ind
        -- BIG BANG
        MP l r     -> S.insert ind $! S.union (mark l) (mark r)
    --
    toList :: S.Set Int -> IO ()
    toList marked = helper 1 M.empty numList
      where
        helper :: Int -> M.Map Int Int -> [(Int, Annotated)] -> IO ()
        helper _ _ [] = return ()
        helper ind re_ind ((i, an):tail) =
          if S.member i marked
            then do
              putStrLn $
                case an of
                  MP r l ->
                    "[" ++
                    show ind ++
                    ". M.P. " ++ show (re_ind M.! l) ++ ", " ++ show (re_ind M.! r) ++ "] " ++ show (mapperE M.! i)
                  Axiom n -> "[" ++ show ind ++ ". Ax. sch. " ++ show n ++ "] " ++ show (mapperE M.! i)
                  Context n -> "[" ++ show ind ++ ". Hypothesis " ++ show n ++ "] " ++ show (mapperE M.! i)
              helper (ind + 1) (M.insert i ind re_ind) tail
            else helper ind re_ind tail
