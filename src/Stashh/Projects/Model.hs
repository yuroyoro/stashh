{-# LANGUAGE OverloadedStrings #-}

module Stashh.Projects.Model where

import Stashh.Table
import Stashh.Model.Type
import qualified Stashh.AnsiColor as C

import Data.Maybe
import Control.Applicative
import qualified Data.Vector as V
import Data.Aeson

----------------------------------------------
-- Projects

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
  , projectKey  :: String
  , projectName :: String
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
    [ ColDesc center "Id"          right C.cyan  (show .projectId)
    , ColDesc center "Key"         left  C.green projectKey
    , ColDesc center "Name"        left  id      projectName
    , ColDesc center "Description" left  id      ((fromMaybe "" .) description)
    , ColDesc center "Personal"    left  id      (show . isPersonal)
    , ColDesc center "Link"        left  id      (linkUrl . link)
    ]

instance PagingDef ProjectsResult where
  paging_start r = start r
  paging_size  r = size r
  paging_limit r = limit r
  paging_last r  = isLastPage r
