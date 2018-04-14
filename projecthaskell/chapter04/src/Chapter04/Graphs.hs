module Chapter04.Graphcs where

import Data.Graph
import Data.Tree

timeMachineGraph :: [(String, String, [String])]
timeMachineGraph = [ ("wood", "wood", ["walls"])
                   , ("plastic", "plastic", ["walls", "wheels"])
                   , ("aliminum", "aluminum", ["wheels", "doors"])
                   , ("walls", "walls", ["done"])
                   , ("wheels", "wheels", ["done"])
                   , ("door", "door", ["done"])
                   , ("done", "done", [])]

timeMachinePrecedence = graphFromEdges timeMachineGraph  


topSortTimeMachine = let (g,v,_) = timeMachinePrecedence
                         in map (\x -> let (k, _, _) = v x in k) $ topSort g


timeMachineTravel :: Graph
timeMachineTravel = buildG (103,2013) [ (1302, 1614)
                                      , (1614, 1302)
                                      , (1302, 2013)
                                      , (2013, 1302)
                                      , (1614, 2013)
                                      , (2013, 1408)
                                      , (1408, 1993)
                                      , (1408, 917)
                                      , (1993, 917)
                                      , (907, 103)
                                      , (103, 917) ]

pathFrom1302to917 = path timeMachineTravel 1302 917

reachable1302 = reachable timeMachineTravel 1302

stronglyConnectedComponents = filter (\(Node { subForest = s } ) -> s /= []) $ scc timeMachineTravel

mp = map flattenSCC $ stronglyConnComp timeMachineGraph
