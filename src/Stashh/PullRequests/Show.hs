{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}
module Stashh.PullRequests.Show where

import Stashh.App
import Stashh.Env
import Stashh.Table
import Stashh.Api
import qualified Stashh.PullRequests.Model as M
import Stashh.Command.Type

import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import qualified Data.Vector as V
import Data.Aeson
import Network.HTTP.Conduit

import System.Console.CmdArgs

data PullRequestShowCommand = PullRequestShowCommand  {
      projectKey     :: String
    , repositorySlug :: String
    , prId           :: String
    , cmdargs        :: [String]
  } deriving (Show, Eq, Data, Typeable)

modePullRequestShow :: Mode (CmdArgs PullRequestShowCommand)
modePullRequestShow =  cmdArgsMode $ PullRequestShowCommand {
  projectKey = ""
    &= argPos 0
    &= typ "PROJECT_KEY"
, repositorySlug = ""
    &= argPos 1
    &= typ "REPOSITORY_SLUG"
, prId = ""
    &= argPos 2
    &= typ "PULL_REQUEST_ID"
, cmdargs = [] &= args
} &= name "pullreqeusts"

mapPullRequestShow :: String -> String -> String -> PullRequestShowCommand -> PullRequestShowCommand
mapPullRequestShow projectKey repositorySlug prId cmd = cmd { projectKey = projectKey, repositorySlug = repositorySlug , prId = prId }

instance Command PullRequestShowCommand where
  runCommand cmd = pullRequest cmd

fetchJson :: Env -> PullRequestShowCommand -> AppT IO M.PullRequest
fetchJson env cmd = do
  request <- prRequest cmd
  liftIO $ fetch env request

fetchPullRequest :: String -> String -> String -> AppT IO M.PullRequest
fetchPullRequest projectKey repositorySlug prId = do
  env     <- ask
  fetchJson env cmd
  where
    cmd = PullRequestShowCommand {
      projectKey = projectKey
    , repositorySlug = repositorySlug
    , prId = prId
    , cmdargs = []
    }

prRequest :: PullRequestShowCommand -> AppT IO (Request m)
prRequest cmd@PullRequestShowCommand{..} = do
  env <- ask
  apiRequest ["/projects", projectKey, "repos", repositorySlug, "pull-requests", prId] Nothing []

pullRequest :: PullRequestShowCommand -> AppT IO ()
pullRequest cmd = do
  env     <- ask
  json    <- fetchJson env cmd
  liftIO $ putStrLn $ M.showPullRequestDetail json

