module Chapter02.Records.Records where

data ClientR = GovOrgR { clientRName :: String }
             | CompanyR { clientRName :: String 
                        , companyID :: Int
                        , person :: PersonR
                        , duty :: String
                        }
             | IndividualR { person :: PersonR }
             deriving Show

data PersonR = PersonR { firstName :: String
                       , lastName :: String
                       } deriving Show

-- Excercise 2-7. Time Machine Records
-- Rewrite the ```TimeMachine``` data type
-- using records

data TimeMachineR = TimeMachineR { manufacturer :: Manufacturer
                                 , model        :: Integer
                                 , name         :: String
                                 , direction    :: Direction
                                 , price        :: Double
                                 } deriving Show
type Manufacturer = String

type Model = Integer

data Direction = Forward | Backward deriving Show

type Price = Double 
