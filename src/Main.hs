{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Stashh.App
import Stashh.Env
import Stashh.Command

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
  dispatch
