{-# LANGUAGE ScopedTypeVariables #-}
module Chapter04.Client where

import Data.Map

import qualified Data.Map as M
import qualified Data.List as L
import Data.Set
import qualified Data.Set as S

data ClientKind = GovOrgKind | CompanyKind | IndividualKind deriving (Eq, Ord, Show)

data Client i = GovOrg { clientId :: i
                       , clientName :: String }
                | Company { clientId :: i
                          , clientName :: String
                          }
                | Individual { clientId :: i
                             , person :: Person
                             }
                deriving (Show, Eq, Ord)

data Person = Person { firstName :: String
                     , lastName :: String
                     } deriving (Show, Eq, Ord)

-- Excercise 4-3
-- Classify Clients Solution a
classifyClientsA :: Ord i => [Client i] -> Map ClientKind (Set (Client i))
classifyClientsA clients =
  let emptyMap = M.empty
      alterFn client clientSet = case clientSet of
                                   Nothing -> Just $ S.singleton client
                                   Just cs -> Just $ S.insert client cs
      addToMap client map = case client of
                              g @ (GovOrg _ _) -> M.alter (alterFn g) GovOrgKind map
                              c @ (Company _ _) -> M.alter (alterFn c) CompanyKind map
                              i @ (Individual _ _) -> M.alter (alterFn i) IndividualKind map
  in
    L.foldr addToMap emptyMap clients


-- classify clients by partition
classifyClientsB :: Ord i => [Client i] -> Map ClientKind (Set (Client i))
classifyClientsB clients = let (a, b, c) = L.foldr appendListsFn ([],[],[]) clients
                               appendListsFn g@(GovOrg _ _) (gs,cs,is) = (g:gs, cs, is)
                               appendListsFn c@(Company _ _) (gs, cs, is) = (gs, c:cs, is)
                               appendListsFn i@(Individual _ _) (gs, cs, is) = (gs, cs, i:is)
                           in
                             M.fromList [ (GovOrgKind, S.fromList a)
                                        , (CompanyKind, S.fromList b)
                                        , (IndividualKind, S.fromList c)]
