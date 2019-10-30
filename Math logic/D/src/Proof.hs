module Proof where

import           Data.Foldable (find)
import           Data.List     (sort, sortBy, subsequences, (\\))
import           Data.Maybe    (fromJust, isJust)
import qualified Data.Set      as S
import           Expression
import           Oladushek

data Hypnotize
  = Yes String
  | No String
  deriving (Show)

fromHyp :: Hypnotize -> String
fromHyp (Yes s) = s
fromHyp (No s)  = s

processHyps :: Expression -> Maybe (Expression, [Hypnotize])
processHyps tgt
  | isJust posit = posit
  | isJust negat = negat
  | otherwise = Nothing
  where
    variants = sortBy (\l r -> compare (length l) (length r)) $ sort $ subsequences $ extractVars tgt
    posit = findMinYes tgt variants
    negat = findMinNo (Not tgt) variants

findMinYes :: Expression -> [[String]] -> Maybe (Expression, [Hypnotize])
findMinYes e variants = do
  hyps <- find (checkTruth e) $ (\h -> toHyps h h) <$> variants
  return (e, hyps)

findMinNo :: Expression -> [[String]] -> Maybe (Expression, [Hypnotize])
findMinNo e variants = do
  hyps <- find (checkTruth e) $ toHyps [] <$> variants
  return (e, hyps)

------
checkTruth :: Expression -> [Hypnotize] -> Bool
checkTruth e hyps = and $ evalExpr e <$> allVariants
  where
    lst = extractVars e \\ (fromHyp <$> hyps)
    allVariants = (++) hyps . toHyps lst <$> subsequences lst

isYes :: Hypnotize -> Bool
isYes (Yes _) = True
isYes _       = False

evalExpr :: Expression -> [Hypnotize] -> Bool
evalExpr e hyps = helper e
  where
    names = fromHyp <$> filter isYes hyps
    helper :: Expression -> Bool
    helper (Var var)       = var `elem` names
    helper (Not (Var var)) = var `notElem` names
    helper (Not n)         = not (helper n)
    helper (l :&: r)       = helper l && helper r
    helper (l :|: r)       = helper l || helper r
    helper (l :->: r)      = not (helper l) || helper r

toHyps :: [String] -> [String] -> [Hypnotize]
toHyps actual =
  map
    (\var ->
       if var `elem` actual
         then Yes var
         else No var)

extractVars :: Expression -> [String]
extractVars = S.toList . helper
  where
    helper :: Expression -> S.Set String
    helper (Var var)  = S.singleton var
    helper (l :&: r)  = S.union (helper l) (helper r)
    helper (l :|: r)  = S.union (helper l) (helper r)
    helper (l :->: r) = S.union (helper l) (helper r)
    helper (Not n)    = helper n

(?:) :: Maybe a -> Maybe a -> Maybe a
jst@(Just _) ?: _ = jst
Nothing ?: mby = mby

---
getProof :: Expression -> [Hypnotize] -> [Expression]
getProof e hs = fst <$> helper hs (extractVars e \\ (fromHyp <$> hs))
  where
    helper :: [Hypnotize] -> [String] -> [ExpInf]
    helper hyps [] = genProof e hyps
    helper hyps (var:tail) = mergeProof (helper (Yes var : hyps) tail) (helper (No var : hyps) tail) var e

genProof :: Expression -> [Hypnotize] -> [ExpInf]
genProof target hyps = helper target
  where
    helper :: Expression -> [ExpInf]
    helper e@(Var var) = [(e, Axiom)]
    helper e@(Not (Var var)) = [(e, Axiom)]
    helper (l :&: r) = helper l ++ helper r ++ andTT l r
    helper (Not (l :&: r)) =
      if not (evalExpr r hyps)
        then helper (Not r) ++ andTF l r
        else helper (Not l) ++ andFT l r
    helper (l :|: r) =
      if evalExpr l hyps
        then helper l ++ orTT l r
        else helper r ++ orFT l r
    helper (Not (l :|: r)) = helper (Not l) ++ helper (Not r) ++ orFF l r
    helper (l :->: r) =
      if not (evalExpr l hyps)
        then helper (Not l) ++ implFT l r
        else helper l ++ helper r ++ implTT l r
    helper (Not (l :->: r)) = helper l ++ helper (Not r) ++ implTF l r
    helper (Not (Not e)) = helper e ++ notNot e

mergeProof :: [ExpInf] -> [ExpInf] -> String -> Expression -> [ExpInf]
mergeProof true false var target =
  deduction (Var var) true ++
  deduction (Not (Var var)) false ++
  (reindexExpInf (Var var, target, undefined) <$>
   [ (ps "(A -> B) -> (!A -> B) -> (A|!A -> B)", Axiom)
   , (ps "(!A -> B) -> (A|!A -> B)", MP $ ps "A -> B")
   , (ps "(A|!A -> B)", MP $ ps "!A -> B")
   ] ++
   disjThird (ps "A") ++ [(ps "B", MP $ ps "A|!A")])
