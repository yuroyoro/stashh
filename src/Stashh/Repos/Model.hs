{-# LANGUAGE OverloadedStrings #-}

module Stashh.Repos.Model where

import Stashh.Table
import Stashh.Model.Type

import Data.Maybe
import Control.Applicative
import qualified Data.Vector as V
import Data.Aeson

----------------------------------------------
-- Repos

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
  , repoName      :: String
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
    [ ColDesc center "Id"          right (show .repoId)
    , ColDesc center "Slug"        left  slug
    , ColDesc center "Name"        left  repoName
    , ColDesc center "ScmId"       left  scmId
    , ColDesc center "StatusMessage" left  statusMessage
    , ColDesc center "Forkable"    left  (showMaybe forkable)
    , ColDesc left   "CloneUrl"    left  cloneUrl
    , ColDesc left   "Link"        left  (linkUrl . link)
    ]

instance PagingDef ReposResult where
  paging_start r = start r
  paging_size  r = size r
  paging_limit r = limit r
  paging_last r  = isLastPage r

