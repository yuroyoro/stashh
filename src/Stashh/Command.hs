{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}
module Stashh.Command(
  CommandArgs(..),
  parseCommand,
) where

import System.Environment
import System.Console.CmdArgs
import Data.Maybe

data CommandArgs =
  ProjectsArgs {
    projectName :: Maybe String
  , permission  :: Maybe String
  }
  |
  ReposArgs{
    projectKey :: String
  }
  deriving (Show, Eq, Data, Typeable)

modeProjects :: CommandArgs
modeProjects =  ProjectsArgs {
  projectName = Nothing
    &= name "name"
    &= explicit
    &= help "porject name"
, permission = Nothing
    &= help "porject permission"
}

modeRepos :: CommandArgs
modeRepos =  ReposArgs {
  projectKey = "PROJECT KEY"
    &= argPos 0
    &= typ "the parent project key"
}

cmdModes :: Mode (CmdArgs CommandArgs)
cmdModes = cmdArgsMode $ modes [  modeProjects
                                , modeRepos
                                ]
parseCommand :: IO CommandArgs
parseCommand = do
  args <- getArgs
  -- If the user did not specify any arguments,  pretend as "--help" was given
  (if null args then withArgs ["--help"] else id) $ cmdArgsRun cmdModes

