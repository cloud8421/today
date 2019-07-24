{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib
  ( defaultTasks
  , Task
  , Tasks
  ) where

import Data.Aeson
import Data.HashMap.Strict
import Data.Text
import GHC.Generics

data Status
  = Pending
  | Progress
  | Done
  | Cancelled
  deriving (Generic, Read, Show, Eq, ToJSON, FromJSON)

data Task =
  Task
    { status :: Status
    , text :: Text
    }
  deriving (Generic, Read, Show, Eq, ToJSON)

type Tasks = HashMap Int Task

defaultTasks :: Tasks
defaultTasks = fromList [(1, Task Pending "Learn how to use t")]
