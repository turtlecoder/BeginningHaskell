{-# LANGUAGE BangPatterns #-}
module ParMonad.Apriori where

import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.Par
import Control.DeepSeq

data Client = GovOrg { clientName::String }
            | Company { clientName :: String, person::Person, duty::String}
            | Individual { person :: Person }
            deriving (Show, Ord, Eq)

data ClientKind = KindGovOrg | KindCompany | KindIndividual deriving (Show, Eq, Ord)

data Person = Person { firstName :: String
                     , lastName :: String
                     , gender :: Gender
                     } deriving (Show, Eq, Ord)
data Gender = Male | Female | UnknownGender deriving (Show, Eq, Ord)

-- Products
data Product = Product { productID :: Integer
                       , productType :: ProductType
                       } deriving (Show, Eq, Ord)

data ProductType = TimeMachine | TravelGuide | Tool | Trip deriving (Show, Eq, Ord)

data PurchaseInfo = InfoClientKind ClientKind
                  | InfoClientDuty String
                  | InfoClientGender Gender
                  | InfoPurchaseProduct Integer
                  | InfoPurchaseProductType ProductType
                  deriving (Show, Eq, Ord)

newtype Transaction = Transaction (Set PurchaseInfo) deriving (Eq, Ord)

newtype FrequentSet = FrequentSet (Set PurchaseInfo ) deriving (Eq, Ord)

instance NFData FrequentSet where
  rnf (FrequentSet !s) = ()

setSupport :: [Transaction ] -> FrequentSet -> Double
setSupport trans (FrequentSet sElts) = let total = length trans
                                           supp = length (filter (\(Transaction tElts) -> sElts `S.isSubsetOf` tElts) trans)
                                       in
                                         fromIntegral supp / fromIntegral total

noDups :: Ord a => [a] -> [a]
noDups as = S.toList.S.fromList $ as

generateL1 :: Double -> [Transaction] -> [FrequentSet]
generateL1 minSupport transactions =
  let c1 = noDups $ concatMap (\(Transaction t) -> map (FrequentSet . S.singleton) $ S.toList t) transactions
      l1NotFiltered = map (\fs -> (fs, setSupport transactions fs > minSupport)) c1
  in
    concatMap (\(fs, b) -> if b then [fs] else []) l1NotFiltered


parGenerateL1 :: Double -> [Transaction] -> [FrequentSet]
parGenerateL1 minSupport transactions = runPar $ do
  let c1 = noDups $ concatMap (\(Transaction t) -> map (FrequentSet . S.singleton ) $ S.toList t) transactions
  l1NotFiltered <- parMap (\fs -> (fs , setSupport transactions fs > minSupport) ) c1
  return $ concatMap (\(fs, b) -> if b then [fs] else []) l1NotFiltered

generateNextLK :: Double -> [Transaction] -> (Int, [FrequentSet]) -> Maybe ([FrequentSet], (Int, [FrequentSet]))
generateNextLK _ _ (_ , []) = Nothing
generateNextLK minSupport transactions (k, lk) =
  let ck1  = noDups $ [ FrequentSet $ a `S.union` b | FrequentSet a <- lk
                                                    , FrequentSet b <- lk
                                                    , S.size ( a `S.intersection` b) == k - 1]
      lk1 = runPar $ filterLK minSupport transactions ck1
  in Just (lk1, (k+1, lk1))

filterLK :: Double -> [Transaction] -> [FrequentSet] -> Par [ FrequentSet]
filterLK minSupport transactions ck =
  let lengthCK = length ck
  in if lengthCK < 5
     then return $ filter (\fs -> setSupport transactions fs > minSupport) ck
     else let (l, r) = splitAt (lengthCK `div` 2) ck
          in do lVar <- spawn $ filterLK minSupport transactions l
                lFiltered <- get lVar
                rVar <- spawn $ filterLK minSupport transactions r
                rFiltered <- get rVar
                return $ lFiltered ++ rFiltered
       
  
