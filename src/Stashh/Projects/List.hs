{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}
module Stashh.Projects.List where

import Stashh.App
import Stashh.Env
import Stashh.Table
import Stashh.Api
import Stashh.Projects.Model
import Stashh.Command.Type

import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import qualified Data.Vector as V
import Data.Aeson
import Network.HTTP.Conduit

import System.Console.CmdArgs

data ProjectsCommand = ProjectsCommand  {
    projectName  :: Maybe String
  , permission   :: Maybe String
  , cmdargs      :: [String]
  }deriving (Show, Eq, Data, Typeable)

modeProjects :: Mode (CmdArgs ProjectsCommand)
modeProjects =  cmdArgsMode $ ProjectsCommand {
  projectName = Nothing
    &= name "name"
    &= explicit
    &= help "project name"
, permission = Nothing
    &= help "project permission"
, cmdargs = [] &= args
} &= name "projects"

instance Command ProjectsCommand where
  runCommand cmd = projects cmd

projectsRequest :: ProjectsCommand -> AppT IO (Request m)
projectsRequest cmd@ProjectsCommand{..} = do
  env <- ask
  apiRequest ["/projects"] Nothing $ queries env
  where
    queries env =
      [ queryItem     "name"       projectName
      , queryItem     "permission" permission
      , queryItemShow "start"      (env_start env)
      , queryItemShow "limit"      (env_limit env)
      ]

projects :: ProjectsCommand -> AppT IO ()
projects cmd = do
  env     <- ask
  request <- projectsRequest cmd
  json    <- liftIO $ fetch env request
  liftIO $ mapM_ putStrLn $ outputs json
  where
    outputs json =
      [ pagingInfo json
      , ""
      , renderTable $ sortJson projectId values json
      ]
