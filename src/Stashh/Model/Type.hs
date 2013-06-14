{-# LANGUAGE OverloadedStrings #-}

module Stashh.Model.Type where

import Control.Applicative
import Data.Maybe
import qualified Data.Vector as V
import Data.Aeson
import Data.List (transpose, intercalate, intersperse, stripPrefix)

data Link = Link
  { linkUrl :: String
  , linkRel :: String
  }
  deriving (Show, Eq)

instance FromJSON Link where
  parseJSON (Object v) = Link
                          <$> v .:  "url"
                          <*> v .:  "rel"
  parseJSON _          = fail "Link"

data Member = Member
  { user :: User
  , role :: String
  , approved ::Bool
  } deriving (Show, Eq)

instance FromJSON Member where
  parseJSON (Object v) = Member
                          <$> v .:  "user"
                          <*> v .:  "role"
                          <*> v .:  "approved"
  parseJSON _          = fail "Member"

data User = User
  { userName     :: String
  , emailAddress :: String
  , userId       :: Int
  , displayName  :: String
  , active       :: Bool
  , userSlug     :: Maybe String
  } deriving (Show, Eq)

instance FromJSON User where
  parseJSON (Object v) = User
                          <$> v .:  "name"
                          <*> v .:  "emailAddress"
                          <*> v .:  "id"
                          <*> v .:  "displayName"
                          <*> v .:  "active"
                          <*> v .:?  "slug"
  parseJSON _          = fail "User"

data Ref = Ref
  { refId :: String
  , repository :: ShortRepository
  } deriving (Show, Eq)

instance FromJSON Ref where
  parseJSON (Object v) = Ref
                          <$> v .:  "id"
                          <*> v .:  "repository"
  parseJSON _          = fail "Ref"

data ShortRepository = ShortRepository
  { repositorySlug :: String
  , repositoryName :: Maybe String
  , shortProject   :: ShortProject
  } deriving (Show, Eq)

instance FromJSON ShortRepository where
  parseJSON (Object v) = ShortRepository
                          <$> v .:  "slug"
                          <*> v .:? "name"
                          <*> v .:  "project"
  parseJSON _          = fail "ShortRepository"

data ShortProject = ShortProject
  { shortPorjectKey :: String
  } deriving (Show, Eq)


instance FromJSON ShortProject where
  parseJSON (Object v) = ShortProject
                          <$> v .:  "key"
  parseJSON _          = fail "ShortProject"

userNameFromMember :: Member -> String
userNameFromMember = userName . user

showRefId :: Ref -> String
showRefId ref = fromMaybe (refId ref) $ stripPrefix "refs/heads/" (refId ref)

