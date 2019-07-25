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
  , age
  , Task(..)
  , Status(..)
  , Tasks
  ) where

import Data.Aeson
import qualified Data.HashMap.Strict as Map
import Data.Hourglass (timeDiff)
import Data.Hourglass.Types.Orphans
import Data.Text
import GHC.Generics
import Time.Types (Elapsed, Seconds)

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
    , lastUpdate :: Elapsed
    }
  deriving (Generic, Read, Show, Eq, ToJSON, FromJSON)

type Tasks = Map.HashMap Int Task

newTaskId :: Tasks -> Int
newTaskId tasks =
  case Map.keys tasks of
    [] -> 1
    keys -> Prelude.maximum keys + 1

addTask :: Tasks -> Text -> Elapsed -> Tasks
addTask tasks text currentTime =
  let taskId = newTaskId tasks
      task = Task Pending text currentTime
   in Map.insert taskId task tasks

removeTask :: Tasks -> Int -> Tasks
removeTask tasks taskId = Map.delete taskId tasks

checkTask :: Tasks -> Int -> Elapsed -> Either String Tasks
checkTask tasks taskId currentTime =
  case Map.lookup taskId tasks of
    Nothing -> Left "Task not found"
    Just task ->
      Right
        (Map.adjust
           (\t -> t {status = Done, lastUpdate = currentTime})
           taskId
           tasks)

age :: Task -> Elapsed -> Seconds
age task currentTime = timeDiff currentTime (lastUpdate task)

defaultTasks :: Elapsed -> Tasks
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
