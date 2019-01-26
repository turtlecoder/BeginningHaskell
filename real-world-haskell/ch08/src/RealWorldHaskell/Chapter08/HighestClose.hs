module RealWorldHaskell.Chapter08.HighestClose where

import qualified Data.ByteString.Lazy.Char8 as L
import Debug.Trace

closing = readPrice . (!!4) . L.split ','

readPrice :: L.ByteString -> Maybe Double
readPrice str =
  case L.readInteger str of
    Nothing -> Nothing
    Just (dollars, rest) ->
      case L.readInteger $ L.tail rest of
        Nothing -> Nothing
        Just (cents, more) ->
          Just ((fromInteger dollars) + (fromInteger cents) * 0.01)

highestClose = maximum . (Nothing:) . map closing . L.lines

highestCloseFrom path = do contents <- L.readFile path                          
                           print $ highestClose contents
