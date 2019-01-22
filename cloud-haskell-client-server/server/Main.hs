module Main where

import Server
import Options.Applicative
import Text.Read (readMaybe)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)

data Options = Options { serverHost :: Maybe String
                       , port :: Maybe Int
                       , chatName :: Maybe String
                       }

main :: IO ()
main = do
  opts <- liftIO (execParser parserInfo)
  case opts of
    Options Nothing _ _ -> putStrLn "Please, provide the server's HOST ..."
    Options _ Nothing _  -> putStrLn "Please, provide the server's PORT ..."
    Options _ _ Nothing -> putStrLn "Please, provide the server's chat-room NAME ..."
    Options (Just host) (Just port) (Just name) -> serveChatRoom host port name


parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionParser)
  ( fullDesc <>
    progDesc "Chat Server ready to listen for client connections ..." <>
    header "chat-server (powered by Cloud Haskell ...")


optionParser :: Parser Options
optionParser = Options <$>
  option (Just <$> str) ( long "host" <>
                          metavar "host" <>
                          value Nothing <>
                          help "The Chat Server's Host") <*>
  option (str >>= parsePort) ( long "port" <>
                               metavar "PORT" <>
                               value Nothing <>
                               help "The Chat server's port" ) <*>
  option (Just <$> str) ( long "room" <>
                          metavar "ROOMNAME" <>
                          value Nothing <>
                          help "The name of the server room")
  where
    parsePort :: Monad m => String -> m (Maybe Int)
    parsePort str = (pure.readMaybe) str
  
  
                         
  
