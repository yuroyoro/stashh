{-# LANGUAGE OverloadedStrings #-}

module Stashh.Projects where

import Stashh.Env
import Stashh.Table
import Stashh.Api
import Stashh.Model

import Data.List
import Data.Ord
import Data.Maybe
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import qualified Data.Vector as V
import Data.Aeson

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

instance StashApi ProjectsResult where
  apiPath = "/projects"
  buildApiRequest env req = req

instance TableDef Project where
  columnsDef =
    [ ColDesc center "id"          right (show .projectId)
    , ColDesc center "key"         left  key
    , ColDesc center "name"        left  name
    , ColDesc center "description" left  ((fromMaybe "" .) description)
    , ColDesc center "personal"    left  (show . isPersonal)
    , ColDesc center "link"        left  (Stashh.Model.url . link)
    ]

projects :: ReaderT Env IO ()
projects = do
  request <- apiRequest "/projects"
  json    <- liftIO $ fetch request
  let projects =  sortBy (comparing projectId) $ V.toList $ values json
  liftIO $ putStrLn $ renderTable projects
