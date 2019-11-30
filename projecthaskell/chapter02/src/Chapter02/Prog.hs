{-# LANGUAGE RecordWildCards #-}

module Chapter02.Prog where

import Chapter02.Records.Records

greet IndividualR { person = PersonR { .. }} = "Hi " ++ firstName
greet CompanyR    { .. }                     = "Hello " ++ clientRName
greet GovOrgR     { }= "Welcome"

example :: [String]
example = ["This is an example", "of", "interactive", "evaluation"]

-- >>> let foo = "foo"
--
-- >>> foo ++ "bar"
-- "foobar"

-- >>> example
-- ["This is an example","of","interactive","evaluation"]


-- >>> 1 + 1
-- 2







