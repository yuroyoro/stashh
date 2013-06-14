{-# LANGUAGE OverloadedStrings #-}

module Stashh.PullRequests.Model where

import Stashh.Table
import Stashh.Model.Type
import qualified Stashh.AnsiColor as C

import Data.List (intersperse)
import Data.Maybe
import Data.Monoid
import Control.Applicative
import qualified Data.Vector as V
import Data.Aeson
import Text.Printf

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
  , description  :: Maybe String
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
                          <*> v .:? "description"
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
    [ ColDesc center "Id"          right C.cyan    (show . prId)
    , ColDesc center "Title"       left  id        (showWithMax 60 title)
    , ColDesc center "Author"      left  C.magenta (userNameFromMember . author)
    , ColDesc center "Reviewers"   left  id        (reviewersState . reviewers)
    , ColDesc center "Source"      left  C.yellow  (showRefWithMax 30 (refId . fromRef))
    , ColDesc center "Destination" left  C.blue    (showRefWithMax 30 (refId . toRef))
    , ColDesc center "Updated"     left  id        (showTimeAgo updatedDate)
    ]

instance PagingDef PullRequestsResult where
  paging_start r = start r
  paging_size  r = size r
  paging_limit r = limit r
  paging_last r  = isLastPage r

reviewersState :: V.Vector Member -> String
reviewersState rs = concat $ intersperse ", " $ V.toList $ V.map reviewerState rs

reviewerState :: Member -> String
reviewerState m = (if (approved m) then C.green  else id) (userNameFromMember m)

pullReqeustTitle :: PullRequest -> String
pullReqeustTitle pr =  "[" <> (showPullReqeustStatus pr) <> "] " <> (showPullReqeustId pr)  <> " " <> (title pr)

showPullReqeustId  :: PullRequest -> String
showPullReqeustId  pr = C.cyan ("#" <> (show $ prId pr))

showPullReqeustStatus :: PullRequest -> String
showPullReqeustStatus pr = case state pr of
  "MERGED"   -> C.green $ state pr
  "OPEN"     -> C.blue  $ state pr
  "DECLINED" -> C.red   $ state pr
  _ -> state pr

showPullRequestDetail :: PullRequest -> String
showPullRequestDetail pr = unlines
  [ ""
  , printf "[%s] %s | %s" (showPullReqeustStatus pr) (showPullReqeustId pr) (title pr)
  , "--------------------------------------------------------------------------------"
  , printf "  version %d" (version pr)
  , printf "  %s -> %s" (C.yellow $ showRefId $ fromRef pr) (C.blue $ showRefId $ toRef pr)
  , printf "  %d Reviews : %s" (V.length $ reviewers pr) (reviewersState $ reviewers pr)
  , printf "  %s created a pull request %s" (C.magenta $ displayName $ user $ author $ pr) (showTimeAgo createdDate pr)
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
  , fromMaybe "" (description pr)
  , ""
  , "--------------------------------------------------------------------------------"
  ]

