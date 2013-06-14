{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}
module Stashh.PullRequests.Decline where

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

data PullRequestDeclineCommand = PullRequestDeclineCommand  {
      projectKey     :: String
    , repositorySlug :: String
    , prId           :: String
    , targetVersion  :: Maybe String
    , cmdargs        :: [String]
  } deriving (Show, Eq, Data, Typeable)

modePullRequestDecline :: Mode (CmdArgs PullRequestDeclineCommand)
modePullRequestDecline =  cmdArgsMode $ PullRequestDeclineCommand {
  projectKey = ""
    &= argPos 0
    &= typ "PROJECT_KEY"
, repositorySlug = ""
    &= argPos 1
    &= typ "REPOSITORY_SLUG"
, prId = ""
    &= argPos 2
    &= typ "PULL_REQUEST_ID"
, targetVersion = Nothing
    &= name "target_version"
    &= explicit
    &= help "the current version of the pull request. If the server's version is newer than the specified version the operation will fail. To determine the current version of the pull request it should be fetched from the server prior to this operation. Look for the 'version' attribute in the returned JSON structure."
, cmdargs = [] &= args
} &= name "decline pullreqeust"

mapPullRequestDecline :: String -> String -> String -> PullRequestDeclineCommand -> PullRequestDeclineCommand
mapPullRequestDecline projectKey repositorySlug prId cmd = cmd { projectKey = projectKey, repositorySlug = repositorySlug , prId = prId }

instance Command PullRequestDeclineCommand where
  runCommand cmd = declinePullRequest cmd

declinePrRequest :: PullRequestDeclineCommand -> AppT IO (Request m)
declinePrRequest cmd@PullRequestDeclineCommand{..} = do
  env <- ask
  apiRequest ["/projects", projectKey, "repos", repositorySlug, "pull-requests", prId, "decline"] Nothing [queryItem "version" targetVersion]

declinePullRequest :: PullRequestDeclineCommand -> AppT IO ()
declinePullRequest cmd = do
  env     <- ask
  json    <- PRS.fetchPullRequest (projectKey cmd) (repositorySlug cmd) (prId cmd)
  request <- declinePrRequest $ cmd { targetVersion =  (targetVersion cmd) <|> (Just $ show $ M.version json)}
  liftIO $ putStrLn $ (C.green "Declined") <> " -> " <> (M.pullReqeustTitle json)
