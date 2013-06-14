{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}
module Stashh.Env(
  Env (..),
  parseEnv,
  filterEnvArgs,
  removeEnvArgs,
  removeOptArgs,
  url,
  user,
  passwd
) where

import System.Environment
import System.Console.CmdArgs
import System.FilePath ((</>), takeDirectory)
import System.Directory

import qualified Data.ByteString.Char8 as B8
import Data.Monoid
import Data.Maybe
import Data.List (isPrefixOf, (\\))
import Control.Applicative
import Control.Monad (filterM, mzero)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Exception as E
import qualified Data.Configurator as Conf

data Env = Env {
  api_url :: Maybe String, username  :: Maybe String, password  :: Maybe String
, debug   :: Bool,         env_start :: Maybe Int,    env_limit :: Maybe Int
} deriving (Show, Eq, Data, Typeable)

envMode :: Mode (CmdArgs Env)
envMode = cmdArgsMode $ Env {
  api_url = Nothing
      &= name "url"
      &= explicit
      &= help "The stash api url (e.g https://stash.atlassian.com)"
, username = Nothing
      &= name "user"
      &= explicit
      &= help "Your username on stash"
, password = Nothing
      &= name "password"
      &= explicit
      &= help "Your password on stash.(for Basic-auth...)"
, debug = False &= help "print debug info"
, env_start = Nothing
    &= name "start"
    &= explicit
    &= help "which item should be used as the first item in the page of results"
, env_limit = Nothing
    &= name "limit"
    &= explicit
    &= help "how many results to return per page"
} &= program "stashh"

removeOptArgs :: [String] -> [String]
removeOptArgs =  filter (not . isPrefixOf "-")

removeEnvArgs :: [String] -> [String]
removeEnvArgs args = args \\ (filterEnvArgs args)

filterEnvArgs :: [String] -> [String]
filterEnvArgs = filter isEnvArgs
  where
    isEnvArgs s =
      (any (flip isPrefixOf s) ["--url", "--user", "--password", "--start", "--limit"]) ||
      (elem s ["--debug", "-d"])

parseEnv :: IO Env
parseEnv = do
  args   <- getArgs
  let envargs = filterEnvArgs args
  e      <- (if null args then withArgs ["--help"] else withArgs envargs) $ cmdArgsRun envMode
  config <- runMaybeT configFilePath
  maybe (return e) (mapConfigToEnv e) config

mapConfigToEnv :: Env -> FilePath -> IO Env
mapConfigToEnv e config_file_path = do
  conf   <- Conf.load [Conf.Required config_file_path]
  url    <- Conf.lookup conf "url"
  user   <- Conf.lookup conf "user"
  passwd <- Conf.lookup conf "password"
  return e { api_url = api_url e <|> url,
    username = username e <|> user,
    password = password e <|> passwd }

configFilePath :: MaybeT IO FilePath
configFilePath = do
  current <- lift getCurrentDirectory
  home    <- lift getHomeDirectory
  searchConfigFile current <|> configFileInDirectory home

searchConfigFile :: FilePath -> MaybeT IO FilePath
searchConfigFile dir = case is_root of
  True  -> mzero
  False -> configFileInDirectory filename <|> searchConfigFile parent
  where
    filename = dir </> stashhConfigFileName
    parent   = takeDirectory dir
    is_root  = dir == parent

configFileInDirectory :: FilePath -> MaybeT IO FilePath
configFileInDirectory filename = MaybeT $ (filterM doesFileExist $ pure filename) >>= return .listToMaybe

stashhConfigFileName :: String
stashhConfigFileName = ".stashh"

url :: Env -> IO B8.ByteString
url e = (do
  return $ B8.pack $ ((fromJust $ api_url e) <> "/rest/api/1.0")
  ) `E.onException` do
    putStrLn "Failed to load settings : url is required"


user :: Env -> IO B8.ByteString
user e = (do
  return $ B8.pack $ fromJust $ username e
  ) `E.onException` do
    putStrLn "Failed to load settings : user is required"

passwd :: Env -> IO B8.ByteString
passwd e = (do
  return $ B8.pack $ fromJust $ password e
  ) `E.onException` do
    putStrLn "Failed to load settings : password is required"


