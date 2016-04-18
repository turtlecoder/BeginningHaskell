module Chapter02.DataTypes where 

data Client =   GovOrg String
              | Company String Integer String String
              | Individual Person Bool
              deriving Show

data Person = Person String String Gender
              deriving Show


data Gender = Male | Female | Unknown
            deriving Show

data Manufacturer = Manufacturer String
                  deriving Show


data TimeMachine = TimeMachine Manufacturer Model Name Spec Price
                   deriving Show

data Model = Model Integer
             deriving Show

data Name = Name String
            deriving Show

data Spec = Forward | Backward
          deriving Show
                       

data Price = Price Double
             deriving Show

data ClientR =   GovOrgR { clientRName :: String}
               | CompanyR { clientRName :: String
                            , companyID :: Integer
                            , personR :: PersonR
                            , duty :: String
                            }
               | IndividualR { personR :: PersonR }
               deriving Show

data PersonR = PersonR { firstRName :: String
                         , lastRName :: String
                         } deriving Show


data TimeMachineR = TimeMachineR { manufacturer :: String
                                   , model :: Integer
                                   , name :: String
                                   , spec :: Spec
                                   , price :: Double
                                   } deriving Show
                              
