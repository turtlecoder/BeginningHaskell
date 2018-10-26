module Main where

import Client
import Options.Applicative
import Data.Monoid ((<>))
import Text.Read (readMaybe)
import Control.Monad.IO.Class (liftIO)

data Options = Options {
  serverAddr :: Maybe String
  , sport :: Maybe Int 
  , host :: Maybe String
  , cport :: Maybe Int
  , chatName :: Maybe String
  }

main :: IO ()
main = do
  opts <- liftIO (execParser parserInfo)
  case opts of
    Options Nothing _ _ _ _ -> putStrLn "Please, provide the server's ADDRESS"
    Options _ Nothing _ _ _ -> putStrLn "Please, provide the server's PORT"
    Options _ _ Nothing _ _ -> putStrLn "Please, provide the client's HOST"
    Options _ _ _ Nothing _ -> putStrLn "Please, provide the client's PORT"
    Options _ _ _ _ Nothing -> putStrLn "Please, provide the client's chatroom NAME"
    Options (Just addr) (Just sport) (Just h) (Just prt) (Just name) -> launchChatClient (addr, sport) (h,prt) name

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionParser)
  (fullDesc <>
   progDesc "Chat Client Ready to connect to a server ... " <>
   header "chat-client - chat client powered by Cloud Haskell")

optionParser :: Parser Options
optionParser =
  Options
  <$> option (Just <$> str)
  ( long     "address"  <>
    metavar  "ADDRESS"  <>
    value    Nothing    <>
    help
    "The chat server's address." )
  <*> option (str >>= parsePort)
  ( long "sport" <>
    metavar "SPORT" <>
    value Nothing <>
    help
    "The chat server's port.")
  <*> option (Just <$> str)
  ( long     "host"   <>
    metavar  "HOST"   <>
    value    Nothing  <>
    help
    "The chat client's host." )
  <*> option (str >>= parsePort)
  ( long     "cport"   <>
    metavar  "CPORT"   <>
    value    Nothing  <>
    help
    "The chat client's port." )
  <*> option (Just <$> str)
  ( long     "room"      <>
    metavar  "ROOMNAME"  <>
    value    Nothing     <>
    help
    "The chat-room we want to connect to." )
  where
    parsePort :: Monad m => String -> m (Maybe Int)
    parsePort = pure . readMaybe
