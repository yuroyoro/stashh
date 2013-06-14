{-# LANGUAGE OverloadedStrings #-}
module Stashh.Command where

import Stashh.App
import Stashh.Env
import Stashh.Command.Type
import Stashh.Projects.List
import Stashh.Repos.List
import Stashh.PullRequests.List
import Stashh.PullRequests.Show
import Stashh.PullRequests.Approve

import System.Environment
import System.Console.CmdArgs

import Data.List (isPrefixOf)

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class (MonadIO, liftIO)

dispatch :: AppT IO ()
dispatch = do
  env  <- ask
  args <- liftIO $  removeEnvArgs <$> getArgs
  debugout env ["-- Args --", show args, ""]
  case removeOptArgs args of
    ("projects" : []) ->
    {- ("projects" : projectId : []) -> -}
      parseAndRun args modeProjects id
    ("projects" : prjKeyg : "repos" : []) ->
      parseAndRun args modeRepos (mapRepos prjKeyg)
    {- ("projects" : projectId : "repos" : repoSlug : []) -> -}
    ("projects" : prjKeyg : "repos" : repoSlug : "pr" : []) ->
      parseAndRun args modePullRequests (mapPullRequests prjKeyg repoSlug)
    ("projects" : prjKeyg : "repos" : repoSlug : "pullrequests" : []) ->
      parseAndRun args modePullRequests (mapPullRequests prjKeyg repoSlug)
    ("projects" : prjKeyg : "repos" : repoSlug : "pr" : prId : []) ->
      parseAndRun args modePullRequestShow (mapPullRequestShow prjKeyg repoSlug prId)
    ("projects" : prjKeyg : "repos" : repoSlug : "pullrequests" : prId : []) ->
      parseAndRun args modePullRequestShow (mapPullRequestShow prjKeyg repoSlug prId)

    {- ("projects" : prjKeyg : "repos" : repoSlug : "pullrequests" : prId : "activities" : []) -> -}
    {- ("projects" : prjKeyg : "repos" : repoSlug : "pullrequests" : prId : "decline" : []) -> -}
    {- ("projects" : prjKeyg : "repos" : repoSlug : "pullrequests" : prId : "merge" : []) -> -}
    ("projects" : prjKeyg : "repos" : repoSlug : "pr" : prId : "approve" : []) ->
      parseAndRun args modePullRequestApprove (mapPullRequestApprove prjKeyg repoSlug prId)
    ("projects" : prjKeyg : "repos" : repoSlug : "pullrequests" : prId : "approve" : []) ->
      parseAndRun args modePullRequestApprove (mapPullRequestApprove prjKeyg repoSlug prId)



    _ -> liftIO $ fail "unknown command"

  {- runCommand cmd -}

parseAndRun :: (Show a, Command a) => [String] -> (Mode (CmdArgs a)) -> (a -> a) -> AppT IO ()
parseAndRun args mode mapper = do
  env  <- ask
  cmd  <- liftIO $ withArgs args $ mapper <$> cmdArgsRun mode
  debugout env ["-- Command --", show cmd, ""]
  runCommand cmd
