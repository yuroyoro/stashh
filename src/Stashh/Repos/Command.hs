{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}
module Stashh.Repos.Command where

import Stashh.App
import Stashh.Env
import Stashh.Table
import Stashh.Api
import Stashh.Repos.Model
import Stashh.Command.Type

import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import qualified Data.Vector as V
import Data.Aeson
import Network.HTTP.Conduit

import System.Console.CmdArgs

data ReposCommand = ReposCommand  {
    projectKey :: String
  , cmdargs    :: [String]
  }deriving (Show, Eq, Data, Typeable)

modeRepos :: Mode (CmdArgs ReposCommand)
modeRepos =  cmdArgsMode $ ReposCommand {
  projectKey = ""
    &= argPos 0
    &= typ "PROJECT_KEY"
, cmdargs = [] &= args
} &= name "repos"

mapRepos :: String -> ReposCommand -> ReposCommand
mapRepos projectKey cmd = cmd { projectKey = projectKey }

instance Command ReposCommand where
  runCommand cmd = repos cmd

reposRequest :: ReposCommand -> AppT IO (Request m)
reposRequest cmd@ReposCommand{..} = do
  env <- ask
  apiRequest ["/projects", projectKey, "repos"] Nothing $ queries env
  where
    queries env =
      [ queryItemShow "start"      (env_start env)
      , queryItemShow "limit"      (env_limit env)
      ]

repos :: ReposCommand -> AppT IO ()
repos cmd = do
  env     <- ask
  request <- reposRequest cmd
  json    <- liftIO $ fetch env request
  liftIO $ mapM_ putStrLn $ outputs json
  where
    outputs json =
      [ pagingInfo json
      , ""
      , renderTable $ sortJson repoId values json
      ]
