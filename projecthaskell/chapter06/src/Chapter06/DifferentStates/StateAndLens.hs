{-# LANGUAGE TemplateHaskell #-}

module Chapter06.DifferentStates.StateAndLens where

import Control.Lens
import Chapter06.KMeansCommon
import qualified Data.Map as M
import Control.Monad.State
import Data.List (minimumBy)
import Data.Char

data KMeansState v = KMeansState { _centroids :: [v]
                                 , _threshold :: Double
                                 , _steps :: Int
                                 }


makeLenses ''KMeansState

clusterAssignments :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignments centrs points =
  let initialMap = M.fromList $ zip centrs (repeat [])
      in foldr (\aPoint accumMap -> let chosenCentroid =
                                          minimumBy (\x y -> compare (distance x $ toVector aPoint)
                                                                     (distance y $ toVector aPoint)) centrs
                               in M.adjust (aPoint:) chosenCentroid accumMap) initialMap points

newCentroids :: (Vector v, Vectorizable e v) => M.Map v [e] -> [v]
-- newCentroids centroidMap = (M.elems . (fmap (centroid.map toVector))) centroidMap
newCentroids centroidMap = let newCentroidList = fmap (centroid.map toVector) centroidMap
                               newElems = M.elems newCentroidList
                               in newElems


kMeans' :: (Vector v, Vectorizable e v) => [e] -> State (KMeansState v) [v]
kMeans' points = do prevCentrs <- use centroids
                    let assignments = clusterAssignments prevCentrs points
                        newCentrs = newCentroids assignments
                    centroids .= newCentrs
                    steps += 1
                    let err = sum $ zipWith distance prevCentrs newCentrs
                    t <- use threshold
                    if err < t then return newCentrs else kMeans' points

data Client i = GovOrg { _identifier :: i
                       , _name :: String
                       }
              | Company { _identifier :: i
                        , _name :: String
                        }
              | Individual { _identifier :: i
                           , _person :: Person
                           }

              deriving Show

data Person = Person { _firstName :: String, _lastName :: String }  deriving Show
                           
makeLenses ''Client
makeLenses ''Person


data ExampleState = ExampleState { _increment :: Int
                                 , _clients :: [Client Int]
                                 }

makeLenses ''ExampleState

fullName :: Simple Lens Person String
fullName = lens (\(Person f l) -> f ++ " " ++ l)
                (\_ newFullName -> case words newFullName of
                                      f:l:_ -> Person f l
                                      _     -> error "Incorrect Name format")


zoomExample :: State ExampleState ()
zoomExample = do n <- use increment
                 zoom (clients.traversed) $ do
                   identifier  += n
                   person.fullName %= map toUpper
