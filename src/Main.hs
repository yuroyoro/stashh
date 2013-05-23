{-# LANGUAGE OverloadedStrings #-}

module Main where


import Env as E

import System.IO


import Data.Maybe
import Data.Monoid
import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B8

import Network.TLS
import Data.Conduit
import Network.HTTP.Conduit
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
    "projects" -> fetchProjects
    _          -> liftIO $ putStrLn "unknown command"

defaultManagerSettings :: ManagerSettings
defaultManagerSettings = def { managerCheckCerts = \ _ _ _-> return CertificateUsageAccept }

apiRequest :: B8.ByteString -> ReaderT Env IO (Request m)
apiRequest path = do
  env <- ask
  url    <- liftIO $ E.url env
  user   <- liftIO $ E.user env
  passwd <- liftIO $ E.passwd env

  (parseUrl $ B8.unpack (url <> path)) >>= return . applyBasicAuth user passwd

fetchProjects :: ReaderT Env IO ()
fetchProjects = do
  env     <- ask
  request <- apiRequest "/projects"
  withManagerSettings defaultManagerSettings $ \ manager -> do
    response <- responseBody <$> http request manager
    response $$+- CB.sinkHandle stdout
