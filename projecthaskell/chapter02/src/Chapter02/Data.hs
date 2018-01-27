module Chapter02.Data where

data Client = GovOrg String
            | Company String Integer Person String
            | Individual Person Bool
            deriving Show


data Person = Person String String Gender
              deriving Show

-- Ex 2-4

data Gender = Male | Female | Other deriving Show

-- Time Machine Data
data TimeMachine = TimeMachine Manufacturer  Model Name Direction Price deriving Show

data Manufacturer = Manufacturer String deriving Show

type Model = Integer

type Price = Double

data Name = Name String deriving Show

data Direction = Forward | Backward deriving Show




