{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Tasks
  ( defaultTasks
  , totalCount
  , countByStatus
  , groupByContext
  , toList
  , addTask
  , removeTask
  , updateTaskStatus
  , age
  , started
  , Context
  , Task(..)
  , Status(..)
  , Tasks
  ) where

import Data.Aeson
import qualified Data.HashMap.Strict as Map
import Data.Hourglass (timeDiff)
import Data.Hourglass.Types.Orphans
import qualified Data.List as L
import Data.Text
import GHC.Generics
import Time.Types (Elapsed, Seconds)

data Status
  = Pending
  | Progress
  | Done
  | Cancelled
  deriving (Generic, Read, Show, Eq, ToJSON, FromJSON)

type Context = Text

data Task =
  Task
    { status :: Status
    , text :: Text
    , lastUpdate :: Elapsed
    , context :: Context
    }
  deriving (Generic, Read, Show, Eq, ToJSON, FromJSON)

type Tasks = Map.HashMap Int Task

newTaskId :: Tasks -> Int
newTaskId tasks =
  case Map.keys tasks of
    [] -> 1
    keys -> Prelude.maximum keys + 1

addTask :: Tasks -> Text -> Elapsed -> Context -> Tasks
addTask tasks text currentTime context =
  let taskId = newTaskId tasks
      task = Task Pending text currentTime context
   in Map.insert taskId task tasks

removeTask :: Tasks -> Int -> Tasks
removeTask tasks taskId = Map.delete taskId tasks

updateTaskStatus :: Status -> Tasks -> Int -> Elapsed -> Either String Tasks
updateTaskStatus newStatus tasks taskId currentTime =
  case Map.lookup taskId tasks of
    Nothing -> Left "Task not found"
    Just task ->
      Right
        (Map.adjust
           (\t -> t {status = newStatus, lastUpdate = currentTime})
           taskId
           tasks)

age :: Task -> Elapsed -> Seconds
age task currentTime = timeDiff currentTime (lastUpdate task)

started :: Task -> Bool
started task = status task `elem` [Pending, Progress]

defaultTasks :: Elapsed -> Tasks
defaultTasks currentTime =
  Map.fromList
    [ (1, Task Done "Install t" currentTime "inbox")
    , (2, Task Pending "Learn how to use t" currentTime "inbox")
    , (3, Task Pending "Clean keyboard" currentTime "work")
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

groupByContext :: Tasks -> Map.HashMap Context Tasks
groupByContext = Map.foldlWithKey' mergeContexts Map.empty
  where
    mergeContexts contexts taskId task =
      Map.alter (mergeTasks taskId task) (context task) contexts
    mergeTasks taskId task maybeOtherTasks =
      case maybeOtherTasks of
        Just otherTasks -> Just (Map.insert taskId task otherTasks)
        Nothing -> Just (Map.fromList [(taskId, task)])

toList :: Map.HashMap k v -> [(k, v)]
toList = Map.toList
