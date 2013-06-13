module Stashh.Command.Type where

import Stashh.App

class Command a where
  runCommand :: (Command a) => a -> AppT IO ()
