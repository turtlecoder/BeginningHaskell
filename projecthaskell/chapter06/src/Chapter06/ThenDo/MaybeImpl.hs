module Chapter06.ThenDo.MaybeImpl where

thenDo :: (Maybe a) -> (a -> Maybe b) -> Maybe b
thenDo Nothing _ = undefined
thenDo (Just a) f = f a

