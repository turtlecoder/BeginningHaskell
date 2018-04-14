module Chapter04.Sets where

import qualified Data.Set as S


-- Insert into an element into a singleton
insertWelcome = S.insert "welcome" $ S.singleton "hello"

-- creating a set directly
helloBye = S.fromList ["hello", "bye", "hello"]


-- Create a list from a set
listFromSet = S.toList $ S.fromList ["duplicate", "boom", "duplicate"]
