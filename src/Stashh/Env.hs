{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}
module Stashh.Env(
  Env (..),
  parseEnv,
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
import Control.Applicative
import Control.Monad (filterM, mzero)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Exception as E
import qualified Data.Configurator as Conf

data Env =
  ProjectsEnv {
     projectName :: Maybe String
   , permission  :: Maybe String
   , api_url   :: Maybe String , username  :: Maybe String , password :: Maybe String
   , env_start :: Maybe Int    , env_limit :: Maybe Int,     debug    :: Bool
  }
  |
  ReposEnv {
     projectKey :: String
   , api_url   :: Maybe String , username  :: Maybe String , password :: Maybe String
   , env_start :: Maybe Int    , env_limit :: Maybe Int,     debug    :: Bool
  }
  |
  PullRequestsEnv {
     projectKey     :: String
   , repositorySlug :: String
   , direction :: Maybe String
   , atBranch  :: Maybe String
   , prState   :: Maybe String
   , prOrder   :: Maybe String
   , api_url   :: Maybe String , username  :: Maybe String , password :: Maybe String
   , env_start :: Maybe Int    , env_limit :: Maybe Int,     debug    :: Bool
  }
  deriving (Show, Eq, Data, Typeable)

modeProjects :: Env
modeProjects =  ProjectsEnv {
  projectName = Nothing
    &= name "name"
    &= explicit
    &= help "porject name"
, permission = Nothing
    &= help "porject permission"
, api_url = Nothing
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
, env_start = Nothing
      &= name "start"
      &= explicit
      &= help "which item should be used as the first item in the page of results"
, env_limit = Nothing
      &= name "limit"
      &= explicit
      &= help "how many results to return per page"
, debug = False &= help "print debug info"
} &= name "projects"

modeRepos :: Env
modeRepos =  ReposEnv {
  projectKey = def
    &= argPos 0
    &= typ "PROJECT_KEY"
, api_url = Nothing
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
, env_start = Nothing
      &= name "start"
      &= explicit
      &= help "which item should be used as the first item in the page of results"
, env_limit = Nothing
      &= name "limit"
      &= explicit
      &= help "how many results to return per page"
, debug = False &= help "print debug info"
} &= name "repos"

modePullRequests:: Env
modePullRequests=  PullRequestsEnv {
  projectKey = def
    &= argPos 0
    &= typ "PROJECT_KEY"
, repositorySlug = def
    &= argPos 1
    &= typ "REPOSITORY_SLUG"


, direction = Nothing
      &= name "direction"
      &= explicit
      &= help "(optional,  defaults to INCOMING) the direction relative to the specified repository. Either INCOMING or OUTGOING"
, atBranch  = Nothing
      &= name "at"
      &= explicit
      &= help "(optional) a specific branch to find pull requests to or from."
, prState   = Nothing
      &= name "state"
      &= explicit
      &= help "(optional,  defaults to OPEN) only pull requests in the specified state will be returned. Either OPEN, DECLINED or MERGED."
, prOrder   = Nothing
      &= name "order"
      &= explicit
      &= help "(optional) the order to return pull requests in,  either OLDEST (as in: 'oldest first') or NEWEST."

, api_url = Nothing
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
, env_start = Nothing
      &= name "start"
      &= explicit
      &= help "which item should be used as the first item in the page of results"
, env_limit = Nothing
      &= name "limit"
      &= explicit
      &= help "how many results to return per page"
, debug = False &= help "print debug info"
} &= name "pullrequests"

envModes :: Mode (CmdArgs Env)
envModes = cmdArgsMode $ (modes [  modeProjects
                                , modeRepos
                                , modePullRequests
                                ] )&= program "stashh"

parseEnv :: IO Env
parseEnv = do
  args   <- getArgs
  e      <- (if null args then withArgs ["--help"] else id) $ cmdArgsRun envModes
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


