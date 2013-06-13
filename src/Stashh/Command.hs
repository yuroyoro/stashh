{-# LANGUAGE OverloadedStrings #-}
module Stashh.Command where

import Stashh.App
import Stashh.Env
import Stashh.Command.Type
import Stashh.Projects.Command
import Stashh.Repos.Command
import Stashh.PullRequests.Command

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
    ("projects" : projectKey : "repos" : []) ->
      parseAndRun args modeRepos (mapRepos projectKey)
    {- ("projects" : projectId : "repos" : repositorySlug : []) -> -}
    ("projects" : projectKey : "repos" : repositorySlug : "pr" : []) ->
      parseAndRun args modePullRequests (mapPullRequests projectKey repositorySlug)
    ("projects" : projectKey : "repos" : repositorySlug : "pullrequests" : []) ->
      parseAndRun args modePullRequests (mapPullRequests projectKey repositorySlug)
      {- cmdArgsRun modePullRequests -}
    _ -> liftIO $ fail "unknown command"

  {- runCommand cmd -}

parseAndRun :: (Show a, Command a) => [String] -> (Mode (CmdArgs a)) -> (a -> a) -> AppT IO ()
parseAndRun args mode mapper = do
  env  <- ask
  cmd  <- liftIO $ withArgs args $ mapper <$> cmdArgsRun mode
  debugout env ["-- Command --", show cmd, ""]
  runCommand cmd
