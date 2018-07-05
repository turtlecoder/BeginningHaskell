import qualified Control.Distributed.Backend.P2P as P2P
import           Control.Monad.Trans (liftIO)
import           Control.Concurrent (threadDelay)
import Control.Distributed.Process.Node (initRemoteTable)

main = P2P.bootstrap "127.0.0.1" "9001" (\_ -> ("127.0.0.1", "9000")) initRemoteTable [P2P.makeNodeId "seedhost:9001"] $ do
  liftIO $ threadDelay 5000000 -- give dispatcher a second to discover other nodes
  P2P.nsendPeers "myService" ("some", "message")
