{-# LANGUAGE OverloadedStrings #-}

module Stashh.PullRequests.Model where

import Stashh.Table
import Stashh.Model.Type

import Data.List (intersperse)
import Data.Maybe
import Data.Monoid
import Control.Applicative
import qualified Data.Vector as V
import Data.Aeson

----------------------------------------------
-- PullRequests

data PullRequestsResult = PullRequestsResult
  { start       :: Int
  , size        :: Int
  , limit       :: Int
  , filter      :: Maybe String
  , isLastPage  :: Bool
  , values      :: V.Vector PullRequest
  } deriving (Show, Eq)

data PullRequest = PullRequest
  { prId         :: Int
  , version      :: Int
  , title        :: String
  , description  :: String
  , state        :: String
  , createdDate  :: Int
  , updatedDate  :: Int
  , fromRef      :: Ref
  , toRef        :: Ref
  , author       :: Member
  , reviewers    :: V.Vector Member
  , participants :: V.Vector Member
  , link         :: Link
  } deriving (Show, Eq)

instance FromJSON PullRequestsResult where
  parseJSON (Object v) = PullRequestsResult
                          <$> v .:  "start"
                          <*> v .:  "size"
                          <*> v .:  "limit"
                          <*> v .:? "filter"
                          <*> v .:  "isLastPage"
                          <*> v .:?  "values" .!= V.empty
  parseJSON _          = fail "PullRequestsResult"

instance FromJSON PullRequest where
  parseJSON (Object v) = PullRequest
                          <$> v .:  "id"
                          <*> v .:  "version"
                          <*> v .:  "title"
                          <*> v .:  "description"
                          <*> v .:  "state"
                          <*> v .:  "createdDate"
                          <*> v .:  "updatedDate"
                          <*> v .:  "fromRef"
                          <*> v .:  "toRef"
                          <*> v .:  "author"
                          <*> v .:? "reviewers" .!= V.empty
                          <*> v .:? "participants" .!= V.empty
                          <*> v .:  "link"
  parseJSON _          = fail "PullRequest"

instance TableDef PullRequest where
  columnsDef =
    [ ColDesc center "Id"          right (show .prId)
    , ColDesc center "Title"       left  (showWithMax 60 title)
    , ColDesc center "Author"      left  (userNameFromMember . author)
    , ColDesc center "Reviewers"   left  (reviewersState . reviewers)
    , ColDesc center "Source"      left  (showRefWithMax 30 (refId . fromRef))
    , ColDesc center "Destination" left  (showRefWithMax 30 (refId . toRef))
    , ColDesc center "Updated"     left  (showTime updatedDate)
    ]

reviewersState :: V.Vector Member -> String
reviewersState rs = concat $ intersperse ", " $ V.toList $ V.map reviewerState rs

reviewerState :: Member -> String
reviewerState m = (if (approved m) then "+" else " ") <> (userNameFromMember m)

instance PagingDef PullRequestsResult where
  paging_start r = start r
  paging_size  r = size r
  paging_limit r = limit r
  paging_last r  = isLastPage r
