module Chapter06.ThenDo.MaybeImpl where

thenDo :: (Maybe a) -> (a -> Maybe b) -> Maybe b
thenDo Nothing _ = Nothing
thenDo (Just a) f = f a

