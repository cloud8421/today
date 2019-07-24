{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib
  ( defaultTasks
  , totalCount
  , doneCount
  , toList
  , addTask
  , Task(..)
  , Status(..)
  , Tasks
  ) where

import Data.Aeson
import qualified Data.HashMap.Strict as Map
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
  deriving (Generic, Read, Show, Eq, ToJSON, FromJSON)

type Tasks = Map.HashMap Int Task

newTaskId :: Tasks -> Int
newTaskId tasks =
  case Map.keys tasks of
    [] -> 1
    keys -> Prelude.maximum keys + 1

addTask :: Tasks -> Text -> Tasks
addTask tasks text =
  let taskId = newTaskId tasks
      task = Task Pending text
   in Map.insert taskId task tasks

defaultTasks :: Tasks
defaultTasks =
  Map.fromList
    [(1, Task Done "Install t"), (2, Task Pending "Learn how to use t")]

totalCount :: Tasks -> Int
totalCount = Map.size

doneCount :: Tasks -> Int
doneCount tasks =
  let operator count Task {status = Done} = count + 1
      operator count task = count
   in Map.foldl' operator 0 tasks

toList :: Tasks -> [(Int, Task)]
toList = Map.toList
