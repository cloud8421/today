{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Tasks
  ( defaultTasks
  , totalCount
  , countByStatus
  , toList
  , addTask
  , removeTask
  , checkTask
  , Task(..)
  , Status(..)
  , Tasks
  ) where

import Data.Aeson
import qualified Data.HashMap.Strict as Map
import Data.Text
import Data.Time.Clock (UTCTime)
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
    , lastUpdate :: UTCTime
    }
  deriving (Generic, Read, Show, Eq, ToJSON, FromJSON)

type Tasks = Map.HashMap Int Task

newTaskId :: Tasks -> Int
newTaskId tasks =
  case Map.keys tasks of
    [] -> 1
    keys -> Prelude.maximum keys + 1

addTask :: Tasks -> Text -> UTCTime -> Tasks
addTask tasks text currentTime =
  let taskId = newTaskId tasks
      task = Task Pending text currentTime
   in Map.insert taskId task tasks

removeTask :: Tasks -> Int -> Tasks
removeTask tasks taskId = Map.delete taskId tasks

checkTask :: Tasks -> Int -> UTCTime -> Either String Tasks
checkTask tasks taskId currentTime =
  case Map.lookup taskId tasks of
    Nothing -> Left "Task not found"
    Just task ->
      Right
        (Map.adjust
           (\t -> t {status = Done, lastUpdate = currentTime})
           taskId
           tasks)

defaultTasks :: UTCTime -> Tasks
defaultTasks currentTime =
  Map.fromList
    [ (1, Task Done "Install t" currentTime)
    , (2, Task Pending "Learn how to use t" currentTime)
    ]

totalCount :: Tasks -> Int
totalCount = Map.size

countByStatus :: Status -> Tasks -> Int
countByStatus s tasks =
  let operator count task =
        if status task == s
          then count + 1
          else count
   in Map.foldl' operator 0 tasks

toList :: Tasks -> [(Int, Task)]
toList = Map.toList
