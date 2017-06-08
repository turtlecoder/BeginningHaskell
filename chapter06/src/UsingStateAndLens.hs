module UsingStateAndLens where

import StateAndLens
import RSData
import Control.Monad.State.Lazy

client1 = Individual 4 (Person "John" "Smith")
client2 = Individual 3 (Person "Albert" "Einstein")

foo = execState zoomExample (ExampleState 2 [client1, client2])

