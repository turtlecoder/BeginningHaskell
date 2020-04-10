module Chapter07.AssocRulesLearning.FlatteningValues where

import Data.Set
import qualified Data.Set as S

-- Clients
data Client = GovOrg { _clientName :: String }
            | Company { _clientName :: String
                      , _person :: Person
                      , _duty :: String
                      }
            | Individual { _person :: Person }
            deriving (Show, Eq, Ord)

data ClientKind = KindGovOrg | KindCompany | KindIndividual deriving (Show, Eq, Ord)

data Person = Person { firstName :: String
                     , lastName :: String
                     , gender :: Gender
                     }
              deriving (Show, Eq, Ord)

data Gender = Male | Female | UnknownGender deriving (Show, Eq, Ord)

-- Products
data Product = Product { productId :: Integer
                       , productType :: ProductType
                       } deriving (Show, Eq, Ord)

data ProductType = TimeMachine | TraveGuide | Tool | Trip deriving (Show, Eq, Ord)

data Purchase = Purchase { client :: Client
                         , products :: [Product]
                         } deriving (Show, Eq, Ord)
                   


data PurchaseInfo = InfoClientKind ClientKind
                  | InfoClientDuty String
                  | InfoClientGender Gender
                  | InfoPurchaseProduct Integer
                  | InfoPurchaseProductType ProductType
                  deriving (Show, Eq, Ord)



newtype Transaction = Transaction (Set PurchaseInfo) deriving (Eq, Ord)

productsToPurchaseInfo :: [Product] -> Set PurchaseInfo
productsToPurchaseInfo =
  Prelude.foldr (\(Product productId productType) purchaseInfo ->
                   S.insert (InfoPurchaseProduct productId) $
                   S.insert (InfoPurchaseProductType productType) purchaseInfo) S.empty


purchaseToTransaction :: Purchase -> Transaction
purchaseToTransaction (Purchase client productList) =
  Transaction $ clientToPurchaseInfo client `S.union` productsToPurchaseInfo productList

-- Exercise 7-3
clientToPurchaseInfo :: Client -> Set PurchaseInfo
clientToPurchaseInfo client = case client of
  GovOrg clientName -> S.fromList [InfoClientKind KindGovOrg]
  Company clientName (Person firstName lastName gender) duty ->
    S.fromList [InfoClientKind KindCompany
               , InfoClientGender gender
               , InfoClientDuty duty]
  Individual (Person firstName lastName gender) ->
    S.fromList [InfoClientKind KindIndividual
               , InfoClientGender gender]
