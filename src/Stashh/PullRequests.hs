{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Stashh.PullRequests where

import Stashh.App
import Stashh.Env
import Stashh.Table
import Stashh.Api
import Stashh.Model

import Data.List (intersperse)
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import qualified Data.Vector as V
import Data.Aeson
import Network.HTTP.Conduit

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
    , ColDesc center "Title"       left  (showWithMax 40 title)
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

prRequest :: Env -> AppT IO (Request m)
prRequest env@PullRequestsEnv {..} = apiRequest ["/projects", projectKey, "repos", repositorySlug, "pull-requests"] Nothing queries
  where
    queries =
      [
        queryItem "direction" direction
      , queryItem "at" atBranch
      , queryItem "state" prState
      , queryItem "order" prOrder
      , queryItemShow "start" env_start
      , queryItemShow "limit" env_limit
      ]
prRequest env = fail ("Invalid Env Type : " <> (show env))

pullRequests :: AppT IO ()
pullRequests = do
  env     <- ask
  request <- prRequest env
  json    <- liftIO $ fetch env request
  liftIO $ mapM_ putStrLn $ outputs json
  where
    outputs json =
      [ pagingInfo json
      , ""
      , renderTable $ sortJson prId values json
      ]
