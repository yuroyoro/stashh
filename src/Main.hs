{-# LANGUAGE OverloadedStrings #-}

module Main where

import Stashh.Env as E
import qualified Stashh.Projects as Projects

import System.IO

import Data.Maybe
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import qualified Data.Conduit.Binary as CB

main = do
  env <- E.parseEnv
  runReaderT execute env

execute :: ReaderT Env IO ()
execute = do
  env <- ask
  liftIO $ putStrLn $ show env
  let cmd = E.command env

  case cmd of
    "projects" -> Projects.projects
    _          -> liftIO $ putStrLn "unknown command"

