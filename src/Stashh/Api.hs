{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Stashh.Api where

import Stashh.Env as E
import Stashh.App as E

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource

import Data.List
import Data.Ord
import Data.Maybe
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as B8

import Network.TLS
import Data.Aeson
import Data.Attoparsec
import Data.Conduit
import Data.Conduit.Attoparsec
import Network.HTTP.Conduit
import Network.HTTP.Types

import Data.Aeson

type QueryEntry = (B8.ByteString, Maybe B8.ByteString)

queryItem :: String -> Maybe String -> QueryEntry
queryItem k mv = (B8.pack k, B8.pack <$> mv)

queryItemShow :: (Show a) => String -> Maybe a -> QueryEntry
queryItemShow k mv = (B8.pack k, (B8.pack . show) <$> mv)

toQueryStrings :: [QueryEntry] -> B8.ByteString
toQueryStrings qs = renderQuery True $ filter (isJust . snd) qs

addSlash :: B8.ByteString -> B8.ByteString
addSlash s = if (B8.isPrefixOf "/" s) then s else "/" <> s

buildRequestPath :: [String] -> Maybe String -> [QueryEntry] -> B8.ByteString
buildRequestPath paths mpath qs = basepath <> extrapath <> toQueryStrings qs
    where
        basepath  = B8.concat $ map (addSlash . B8.pack) paths
        extrapath = maybe B8.empty (addSlash . B8.pack) $ mpath

defaultManagerSettings :: ManagerSettings
defaultManagerSettings = def { managerCheckCerts = \ _ _ _-> return CertificateUsageAccept }

apiRequest :: [String] -> Maybe String -> [QueryEntry] -> AppT IO (Request m)
apiRequest paths mpath qs = do
  env    <- ask
  url    <- liftIO $ E.url env
  user   <- liftIO $ E.user env
  passwd <- liftIO $ E.passwd env

  (parseUrl $ B8.unpack (url <> path)) >>= return . applyBasicAuth user passwd
  where path = buildRequestPath paths mpath qs

fetch :: (FromJSON a,
          MonadIO m,
          MonadBaseControl IO m,
          MonadThrow m,
          MonadUnsafeIO m) => Env -> Request (ResourceT m) -> m a
fetch env request = do
  debugout env ["-- Request --", show request, ""]
  withManagerSettings defaultManagerSettings $ \ manager -> do
    response <- responseBody <$> http request manager
    j <- response $$+- sinkParser json
    debugout env ["-- Json --", show j, ""]
    case fromJSON j of
      Error msg -> fail ("JSON parse error: " <> msg <> (" : json -> " <> (show j)))
      Success res -> return res

sortJson :: Ord a => (b -> a) -> (t -> V.Vector b) -> t -> [b]
sortJson f g json = sortBy (comparing f) $ V.toList $ g json
