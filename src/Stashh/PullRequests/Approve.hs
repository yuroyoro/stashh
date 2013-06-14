{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}
module Stashh.PullRequests.Approve where

import Stashh.App
import Stashh.Env
import Stashh.Table
import Stashh.Api
import qualified Stashh.Model.Type as MT
import qualified Stashh.PullRequests.Model as M
import qualified Stashh.PullRequests.Show as PRS
import Stashh.Command.Type
import qualified Stashh.AnsiColor as C

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

data PullRequestApproveCommand = PullRequestApproveCommand  {
      projectKey     :: String
    , repositorySlug :: String
    , prId           :: String
    , cmdargs        :: [String]
  } deriving (Show, Eq, Data, Typeable)

modePullRequestApprove :: Mode (CmdArgs PullRequestApproveCommand)
modePullRequestApprove =  cmdArgsMode $ PullRequestApproveCommand {
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
} &= name "approve pullreqeust"

mapPullRequestApprove :: String -> String -> String -> PullRequestApproveCommand -> PullRequestApproveCommand
mapPullRequestApprove projectKey repositorySlug prId cmd = cmd { projectKey = projectKey, repositorySlug = repositorySlug , prId = prId }

instance Command PullRequestApproveCommand where
  runCommand cmd = approvePullRequest cmd

approvePrRequest :: PullRequestApproveCommand -> AppT IO (Request m)
approvePrRequest cmd@PullRequestApproveCommand{..} = do
  env <- ask
  apiRequest ["/projects", projectKey, "repos", repositorySlug, "pull-requests", prId, "approve"] Nothing []

approvePullRequest :: PullRequestApproveCommand -> AppT IO ()
approvePullRequest cmd = do
  env     <- ask
  request <- approvePrRequest cmd
  memberJson <- liftIO $ post env request Nothing :: AppT IO MT.Member
  json    <- PRS.fetchPullRequest (projectKey cmd) (repositorySlug cmd) (prId cmd)
  liftIO $ putStrLn $ (C.green "Apporved") <> " -> " <> (M.pullReqeustTitle json)
