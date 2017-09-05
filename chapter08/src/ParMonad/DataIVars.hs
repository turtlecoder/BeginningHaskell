module ParMonad.DataIVars where

import Control.Monad.Par

printTicket :: Int -> Int -> [(Int, String)] -> [(Int, String)]
printTicket idC idP clients products = runPar $ do
  clientV <- new
  productV <- new
  fork $ lookupPar clientV idC clients
  fork $ lookupPar productV idP products
  envV <- new
  letterV <- new
  fork $ printEnvelope clientV envV
  fork $ printLetter clientV productV letterV
  envS <- get envV
  letterS <- get letterV
  return $ envS ++ "\n\n" ++ letterS

lookupPar :: (Eq a, NFData b) => IVar (Maybe b) -> a -> [(a,b)] -> Par ()
lookupPar i  _ []  = put i Nothing
lookupPar i x ((k,v) : r ) | x == k = put i $ Just v
                           | otherwise = lookupPar i x r


printEnvelope :: IVar (Maybe String ) -> IVar String -> Par ()
printEnvelope clientV envV = do clientName <- get clientV
                                case clientName of
                                  Nothing <- put envV "Unknown"
                                  Just n <- put envV $ "To: " ++ n


printLetter :: IVar (Maybe String ) -> IVar (Maybe String) -> IVar String -> Par()
printLetter clientV productV letterV = do clientName <- get clientV
                                          productName <- get productV
                                          case (clientName, productName ) of
                                            (Nothing, Nothing) -> put letterV "Unknown"
                                            (Just n , Nothing) -> put letterV $ n ++ " bought something"
                                            (Nothing, Just p ) -> put letterV $ "Someone bought " ++ p
                                              (Just n, Just p) -> put letterV $ n ++ " bought " ++ p

  
