{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Stashh.Projects where

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

data ProjectsResult = ProjectsResult
  { start       :: Int
  , size        :: Int
  , limit       :: Int
  , filter      :: Maybe String
  , isLastPage  :: Bool
  , values      :: V.Vector Project
  }
  deriving (Show, Eq)

data Project = Project
  { projectId   :: Int
  , key         :: String
  , name        :: String
  , description :: Maybe String
  , isPersonal  :: Bool
  , link        :: Link
  }
  deriving (Show, Eq)

instance FromJSON ProjectsResult where
  parseJSON (Object v) = ProjectsResult
                          <$> v .:  "start"
                          <*> v .:  "size"
                          <*> v .:  "limit"
                          <*> v .:? "filter"
                          <*> v .:  "isLastPage"
                          <*> v .:?  "values" .!= V.empty
  parseJSON _          = fail "ProjectsResult"

instance FromJSON Project where
  parseJSON (Object v) = Project
                          <$> v .:  "id"
                          <*> v .:  "key"
                          <*> v .:  "name"
                          <*> v .:? "description"
                          <*> v .:  "isPersonal"
                          <*> v .:  "link"
  parseJSON _          = fail "Project"

instance TableDef Project where
  columnsDef =
    [ ColDesc center "id"          right (show .projectId)
    , ColDesc center "key"         left  key
    , ColDesc center "name"        left  name
    , ColDesc center "description" left  ((fromMaybe "" .) description)
    , ColDesc center "personal"    left  (show . isPersonal)
    , ColDesc center "link"        left  (Stashh.Model.url . link)
    ]

instance PagingDef ProjectsResult where
  paging_start r = start r
  paging_size  r = size r
  paging_limit r = limit r

projectsRequest :: Env -> AppT IO (Request m)
projectsRequest env@ProjectsEnv {..} = apiRequest ["/projects"] Nothing queries
  where
    queries =
      [ queryItem     "name"       projectName
      , queryItem     "permission" permission
      , queryItemShow "start"      env_start
      , queryItemShow "limit"      env_limit
      ]
projectsRequest env = fail ("Invalid Env Type : " <> (show env))

projects :: AppT IO ()
projects = do
  env     <- ask
  request <- projectsRequest env
  json    <- liftIO $ fetch env request
  liftIO $ mapM_ putStrLn $ outputs json
  where
    outputs json =
      [ pagingInfo json
      , ""
      , renderTable $ sortJson projectId values json
      ]
