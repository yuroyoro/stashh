{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
module Stashh.App where

import Stashh.Env

import System.IO

import Data.Maybe
import Control.Applicative
import Control.Monad.Reader

newtype AppT m a = AppT { runAppT:: ReaderT Env m a}
 deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadReader Env)

runApp :: Monad m => AppT m a -> Env -> m a
runApp act env = runReaderT (runAppT act) env

debugout :: (MonadIO m) => Env -> [String] -> m ()
debugout env vs = when (debug env) $ liftIO $ mapM_ putStrLn vs


