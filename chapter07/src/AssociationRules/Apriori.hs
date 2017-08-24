

module AssociationRules.Apriori where

import AssociationRules.FlatteningValuesTransactions
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad
import Data.List

newtype FrequentSet = FrequentSet (Set PurchaseInfo) deriving (Eq, Ord)

data AssocRule = AssocRule (Set PurchaseInfo ) (Set PurchaseInfo) deriving (Eq,Ord)

instance Show AssocRule where
  show (AssocRule a b) = show a ++ " => " ++ show b


setSupport :: [Transaction] -> FrequentSet -> Double
setSupport trans (FrequentSet sElts) = let total = length trans
                                           supp = length (filter (\(Transaction tElts) -> sElts `S.isSubsetOf` tElts) trans)
                                       in fromIntegral supp / fromIntegral total


ruleConfidence :: [Transaction] -> AssocRule -> Double
ruleConfidence trans (AssocRule a b ) = setSupport trans (FrequentSet $ a `S.union` b) / setSupport trans (FrequentSet a)

generateL1 :: Double -> [Transaction] -> [FrequentSet]
generateL1 minSupport transactions = noDups $ do Transaction t <- transactions
                                                 e <- S.toList t
                                                 let fs = FrequentSet $ S.singleton e
                                                 guard $ setSupport transactions fs > minSupport
                                                 return fs
                                                 
noDups :: Ord a => [a] -> [a]
noDups = S.toList . S.fromList

generateNextLK :: Double -> [Transaction] -> (Int, [FrequentSet]) -> Maybe ([FrequentSet], (Int, [FrequentSet]))
generateNextLK _ _ (_, []) = Nothing
generareNextLK minSupport transactions (k, lk) =
  let lk1 = noDups $ do FrequentSet a <- lk
                        FrequentSet b <- lk
                        guard $ S.size (a `S.intersection` b ) == k-1
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


powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)


apriori :: Double -> Double -> [Transaction] -> [AssocRule]
apriori minSupport minConfidence transactions =
  generateAssocRules minConfidence transactions $ concat $ unfoldr (generateNextLK minSupport transactions) ( 1, generateL1 minSupport transactions)
  
  
