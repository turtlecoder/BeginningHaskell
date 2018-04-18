{-# LANGUAGE RecordWildCards #-}

module Chapter02.Prog where

import Chapter02.Records.Records

greet IndividualR { person = PersonR { .. }} = "Hi " ++ firstName
greet CompanyR    { .. }                     = "Hello " ++ clientRName
greet GovOrgR     { }                        = "Welcome"


