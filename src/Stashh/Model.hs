{-# LANGUAGE OverloadedStrings #-}

module Stashh.Model where

import Control.Applicative
import qualified Data.Vector as V
import Data.Aeson

data Link = Link
  { url :: String
  , rel :: String
  }
  deriving (Show, Eq)

instance FromJSON Link where
  parseJSON (Object v) = Link
                          <$> v .:  "url"
                          <*> v .:  "rel"
  parseJSON _          = fail "Link"
