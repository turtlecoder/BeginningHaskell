module Chapter06.ThenDo where

thenDo :: (Maybe a) -> (a -> Maybe b) -> Maybe b
thenDo Nothing _ = undefined
thenDo (Just a) f = f a

