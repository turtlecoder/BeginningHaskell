module Chapter07.AssocRulesLearning.Apriori where

import qualified Data.Set as S
import Chapter07.AssocRulesLearning.FlatteningValues
import Control.Monad
import Data.List(unfoldr)

newtype FrequentSet = FrequentSet (S.Set PurchaseInfo) deriving (Eq, Ord)

-- Association Rule : Antecdent => Consequent
data AssocRule = AssocRule (S.Set PurchaseInfo) (S.Set PurchaseInfo) deriving (Eq, Ord)

instance Show AssocRule where
  show (AssocRule antecedent consequent) = show antecedent ++ " => " ++ show consequent

-- Support of the rule is defined at the ratio of transactions that contain the item with
-- the total transactions
setSupport :: [Transaction] -> FrequentSet -> Double
setSupport transactions (FrequentSet sElements) =
  let total = length transactions
      supp = length (filter (\(Transaction tElements) -> sElements `S.isSubsetOf` tElements ) transactions)
      in fromIntegral supp / fromIntegral total


-- the confidence of a rule A=>C is defined as support(A and C) / support (A)
-- Quanifies  how many transactions support the rule by looking at the ratio of those transactions
-- that fulfill the entire rule and the rules that fulfill just the antecdent 
ruleConfidence :: [Transaction] -> AssocRule -> Double
ruleConfidence transactions (AssocRule antecedent consequent) =
  setSupport transactions (FrequentSet $ antecedent `S.union` consequent) / setSupport transactions (FrequentSet antecedent)


-- Generate the initial Set  of one element that are frequent
generateL1::Double -> [Transaction] -> [FrequentSet]
generateL1 minSupport transactions = noDups $ do Transaction txSet <- transactions
                                                 e <- S.toList txSet
                                                 let fs = FrequentSet $ S.singleton e
                                                 guard $ setSupport transactions fs > minSupport
                                                 return fs

generateNextLk :: Double -> [Transaction] -> (Int, [FrequentSet]) -> Maybe ([FrequentSet], (Int, [FrequentSet]))
generateNextLk _ _ (_, []) = Nothing
generateNextLk minSupport transactions (k, lk)=
  let lk1 = noDups $ do FrequentSet a <- lk
                        FrequentSet b <- lk
                        guard $ S.size (a `S.intersection` b ) == k - 1
                        let fs = FrequentSet $ a `S.union` b
                        guard $ setSupport transactions fs > minSupport
                        return fs
  in Just (lk1, (k+1, lk1))


generateAssocRules :: Double -> [Transaction] -> [FrequentSet] -> [AssocRule]
generateAssocRules minConfidence transactions sets =
  do FrequentSet fs <- sets
     subset@(_:_) <- powerset $ S.toList fs
     let ssubset = S.fromList subset
         rule = AssocRule ssubset (fs `S.difference` ssubset)
     guard $ ruleConfidence transactions rule > minConfidence
     return rule

apriori :: Double -> Double -> [Transaction] -> [AssocRule]
apriori minSupport minConfidence transactions =
  generateAssocRules minConfidence transactions $ concat $ unfoldr (generateNextLk minSupport transactions) (1, generateL1 minSupport transactions)

-- Remove duplicates
noDups :: Ord a => [a] -> [a]
noDups = S.toList . S.fromList

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)   
