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
                deriving (Show, Ord)

data Person = Person { firstName :: String
                     , lastName :: String
                     } deriving (Show, Ord)

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


-- Excercise 4-5: The Same Client
instance (Eq i ) =>Eq (Client i) where
  (==) (GovOrg { clientId=cid1, clientName = cn1 }) (GovOrg { clientId = cid2 , clientName = cn2 }) = cid1 == cid2 && cn1==cn2
  (==)
    (Company { clientId = cid1, clientName = co1 })
    (Company { clientId = cid2 , clientName = co2}) = cid1==cid2 && co1 == co2
  (==)
    (Individual { clientId = cid1, person = p1 })
    (Individual { clientId = cid2, person = p2 }) = cid1 == cid2 && p1 == p2
  (==) _ _ = undefined

instance Eq Person where
  (==) (Person { firstName = fn1, lastName = ln1 }) (Person { firstName = fn2, lastName = ln2 }) = fn1==fn2 && ln1==ln2
