{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}
module Stashh.PullRequests.Command where

import Stashh.App
import Stashh.Env
import Stashh.Table
import Stashh.Api
import Stashh.PullRequests.Model
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

data PullRequestsCommand = PullRequestsCommand  {
      projectKey     :: String
    , repositorySlug :: String
    , direction      :: Maybe String
    , atBranch       :: Maybe String
    , prState        :: Maybe String
    , prOrder        :: Maybe String
    , cmdargs        :: [String]
  } deriving (Show, Eq, Data, Typeable)

modePullRequests :: Mode (CmdArgs PullRequestsCommand)
modePullRequests =  cmdArgsMode $ PullRequestsCommand {
  projectKey = ""
    &= argPos 0
    &= typ "PROJECT_KEY"
, repositorySlug = ""
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

, cmdargs = [] &= args
} &= name "pullreqeusts"

mapPullRequests :: String -> String -> PullRequestsCommand -> PullRequestsCommand
mapPullRequests projectKey repositorySlug cmd = cmd { projectKey = projectKey, repositorySlug = repositorySlug }

instance Command PullRequestsCommand where
  runCommand cmd = pullRequests cmd

prRequest :: PullRequestsCommand -> AppT IO (Request m)
prRequest cmd@PullRequestsCommand{..} = do
  env <- ask
  apiRequest ["/projects", projectKey, "repos", repositorySlug, "pull-requests"] Nothing $ queries env
  where
    queries env =
      [
        queryItem     "direction" direction
      , queryItem     "at"        atBranch
      , queryItem     "state"     prState
      , queryItem     "order"     prOrder
      , queryItemShow "start"     (env_start env)
      , queryItemShow "limit"     (env_limit env)
      ]

pullRequests :: PullRequestsCommand -> AppT IO ()
pullRequests cmd = do
  env     <- ask
  request <- prRequest cmd
  json    <- liftIO $ fetch env request
  liftIO $ mapM_ putStrLn $ outputs json
  where
    outputs json =
      [ pagingInfo json
      , ""
      , renderTable $ sortJson prId values json
      ]
