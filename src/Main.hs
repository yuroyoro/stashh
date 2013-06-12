{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Stashh.App
import Stashh.Env
import qualified Stashh.Projects as Projects
import qualified Stashh.Repos as Repos

import Control.Monad.Reader
import Control.Monad.IO.Class (MonadIO, liftIO)

main :: IO ()
main = do
  env <- parseEnv
  runApp app env

app :: AppT IO ()
app = do
  env <- ask
  debugout env ["-- Env --", show env, ""]

  case env of
    env@ProjectsEnv {..} -> Projects.projects
    env@ReposEnv {..}    -> Repos.repos

