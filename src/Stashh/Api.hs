{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Stashh.Api where

import Stashh.Env as E

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource

import Data.Monoid
import qualified Data.ByteString.Char8 as B8

import Network.TLS
import Data.Aeson
import Data.Attoparsec
import Data.Conduit
import Data.Conduit.Attoparsec
import Network.HTTP.Conduit

import Data.Aeson

class (FromJSON m) => StashApi m where
  apiPath         :: (StashApi m) => B8.ByteString
  buildApiRequest :: (StashApi m) => E.Env -> Request a -> Request a

defaultManagerSettings :: ManagerSettings
defaultManagerSettings = def { managerCheckCerts = \ _ _ _-> return CertificateUsageAccept }

apiRequest :: B8.ByteString -> ReaderT E.Env IO (Request m)
apiRequest path = do
  env    <- ask
  url    <- liftIO $ E.url env
  user   <- liftIO $ E.user env
  passwd <- liftIO $ E.passwd env

  (parseUrl $ B8.unpack (url <> path)) >>= return . applyBasicAuth user passwd


fetch :: (FromJSON a,
          MonadIO m,
          MonadBaseControl IO m,
          MonadThrow m,
          MonadUnsafeIO m) => Request (ResourceT m) -> m a
fetch request = do
  withManagerSettings defaultManagerSettings $ \ manager -> do
    response <- responseBody <$> http request manager
    j <- response $$+- sinkParser json
    case fromJSON j of
      Error msg -> fail ("JSON parse error: " <> msg <> ("json -> " <> (show j)))
      Success res -> return res
