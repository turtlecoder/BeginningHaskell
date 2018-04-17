import Control.Monad.Writer

accessDatabase:: Writer String  ()
accessDatabase = do tell "Start database access"
                    info <- readInformation
                    computeValue info
                    tell "Finish database access"

readInformation::Writer String Integer
readInformation = return 123


computeValue::Integer -> Writer String Integer
computeValue x = return (x+1)

