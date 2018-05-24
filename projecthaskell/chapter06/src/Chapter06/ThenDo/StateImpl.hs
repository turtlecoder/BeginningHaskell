module Chapter06.ThenDo.StateImpl where

type State s a = s -> (a,s)

-- This version is easier to grok, than the one give in the book
thenDo :: State s a -> (a -> State s b) -> State s b
thenDo currentState nextState = \s -> let (resultOfS, stateAfterS) = currentState s
                                      in nextState resultOfS stateAfterS

-- This is the versions in the book
thenDoUncurried :: (s -> (a,s)) -> (a -> s -> (b,s)) -> (s-> (b,s))
thenDoUncurried f g s = let (resultOfF, stateAfterF) = f s
                        in g resultOfF stateAfterF
