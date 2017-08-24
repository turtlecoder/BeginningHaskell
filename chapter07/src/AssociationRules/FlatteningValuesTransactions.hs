module AssociationRules.FlatteningValuesTransactions where

import Data.Set (Set)
import qualified Data.Set as S

data Client = GovOrg { clientName :: String }
            | Company { clientName :: String
                      , person :: Person
                      , duty :: String
                      }
            | Individual { person :: Person }
            deriving (Show, Ord, Eq)

data ClientKind = KindGovOrg | KindCompany | KindIndividual deriving (Show, Ord, Eq)

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

-- Purchase

data Purchase = Purchase { client :: Client, products :: [Product] } deriving (Show, Eq, Ord)


data PurchaseInfo = InfoClientKind ClientKind
                   | InfocClientDuty String
                   | InfoClientGender Gender
                   | InfoPurchasedProduct Integer
                   | InfoPurchasedProductType ProductType
                   deriving (Show, Eq, Ord)

newtype Transaction  = Transaction (Set PurchaseInfo) deriving (Eq, Ord)


productsToPurchaseInfo :: [Product] -> Set PurchaseInfo
productsToPurchaseInfo = foldr (\(Product i t) pinfos -> S.insert (InfoPurchasedProduct i) $ S.insert (InfoPurchasedProductType t) pinfos) S.empty

purchaseToTransaction :: Purchase -> Transaction
purchaseToTransaction (Purchase c p) = Transaction $ clientToPurchaseInfo c `S.union` productsToPurchaseInfo p

-- Excercise 7-3

clientToPurchaseInfo :: Client -> Set PurchaseInfo
clientToPurchaseInfo (GovOrg cn) = S.fromList [InfoClientKind KindGovOrg]
clientToPurchaseInfo (Company cn p d) = S.fromList [InfoClientKind KindCompany, InfocClientDuty d]
clientToPurchaseInfo (Individual (Person fn ln gen ))  = S.fromList [InfoClientKind KindIndividual, InfoClientGender gen]

                                


              
            

