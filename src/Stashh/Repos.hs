{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Stashh.Repos where

import Stashh.App
import Stashh.Env
import Stashh.Table
import Stashh.Api
import Stashh.Model

import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import qualified Data.Vector as V
import Data.Aeson
import Network.HTTP.Conduit

data ReposResult = ReposResult
  { start       :: Int
  , size        :: Int
  , limit       :: Int
  , filter      :: Maybe String
  , isLastPage  :: Bool
  , values      :: V.Vector Repo
  }
  deriving (Show, Eq)

data Repo = Repo
  { repoId        :: Int
  , slug          :: String
  , name          :: String
  , scmId         :: String
  , state         :: String
  , statusMessage :: String
  , forkable      :: Maybe Bool
  {- , project    :: Project -}
  , cloneUrl      :: String
  , link          :: Link
  }
  deriving (Show, Eq)

instance FromJSON ReposResult where
  parseJSON (Object v) = ReposResult
                          <$> v .:  "start"
                          <*> v .:  "size"
                          <*> v .:  "limit"
                          <*> v .:? "filter"
                          <*> v .:  "isLastPage"
                          <*> v .:?  "values" .!= V.empty
  parseJSON _          = fail "ReposResult"

instance FromJSON Repo where
  parseJSON (Object v) = Repo
                          <$> v .:  "id"
                          <*> v .:  "slug"
                          <*> v .:  "name"
                          <*> v .:  "scmId"
                          <*> v .:  "state"
                          <*> v .:  "statusMessage"
                          <*> v .:? "forkable"
                          <*> v .:  "cloneUrl"
                          <*> v .:  "link"
  parseJSON _          = fail "Repo"

instance TableDef Repo where
  columnsDef =
    [ ColDesc center "id"          right (show .repoId)
    , ColDesc center "slug"        left  slug
    , ColDesc center "name"        left  name
    , ColDesc center "scmId"       left  scmId
    , ColDesc center "statusMessage" left  statusMessage
    , ColDesc center "forkable"    left  (showMaybe forkable)
    , ColDesc left   "cloneUrl"    left  cloneUrl
    , ColDesc left   "link"        left  (Stashh.Model.url . link)
    ]

instance PagingDef ReposResult where
  paging_start r = start r
  paging_size  r = size r
  paging_limit r = limit r

reposRequest :: Env -> AppT IO (Request m)
reposRequest env@ReposEnv {..} = apiRequest ["/projects", projectKey, "repos"] Nothing queries
  where
    queries =
      [ queryItemShow "start" env_start
      , queryItemShow "limit" env_limit
      ]
reposRequest env = fail ("Invalid Env Type : " <> (show env))

repos :: AppT IO ()
repos = do
  env     <- ask
  request <- reposRequest env
  json    <- liftIO $ fetch env request
  liftIO $ mapM_ putStrLn $ outputs json
  where
    outputs json =
      [ pagingInfo json
      , ""
      , renderTable $ sortJson repoId values json
      ]
