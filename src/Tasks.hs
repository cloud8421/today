{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Tasks where

import Data.Aeson
import qualified Data.HashMap.Strict as Map
import Data.Hourglass (timeDiff)
import Data.Hourglass.Types.Orphans
import Data.Text (Text)
import GHC.Generics
import Refs (Ref, extractRefs)
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

type TaskId = Int

type Tasks = Map.HashMap TaskId Task

emptyTasks :: Tasks
emptyTasks = Map.empty

defaultContext :: String
defaultContext = "inbox"

newTaskId :: Tasks -> TaskId
newTaskId tasks =
  case Map.keys tasks of
    [] -> 1
    keys -> Prelude.maximum keys + 1

add :: Text -> Elapsed -> Context -> Tasks -> Tasks
add text currentTime context tasks =
  let taskId = newTaskId tasks
      task = Task Pending text currentTime context
   in Map.insert taskId task tasks

remove :: Tasks -> TaskId -> Tasks
remove = flip Map.delete

clearCompleted :: Tasks -> Tasks
clearCompleted = Map.filter (\t -> status t `elem` [Pending, Progress])

updateTask :: (Task -> Task) -> TaskId -> Tasks -> Either String Tasks
updateTask updateFn taskId tasks =
  case Map.lookup taskId tasks of
    Nothing -> Left "Task not found"
    Just task -> Right (Map.adjust updateFn taskId tasks)

updateStatus :: Status -> Tasks -> TaskId -> Elapsed -> Either String Tasks
updateStatus newStatus tasks taskId currentTime =
  updateTask
    (\t -> t {status = newStatus, lastUpdate = currentTime})
    taskId
    tasks

updateText :: Text -> Tasks -> TaskId -> Elapsed -> Either String Tasks
updateText newText tasks taskId currentTime =
  updateTask (\t -> t {text = newText, lastUpdate = currentTime}) taskId tasks

updateContext :: Context -> Tasks -> TaskId -> Elapsed -> Either String Tasks
updateContext newContext tasks taskId currentTime =
  updateTask
    (\t -> t {context = newContext, lastUpdate = currentTime})
    taskId
    tasks

age :: Task -> Elapsed -> Seconds
age task currentTime = timeDiff currentTime (lastUpdate task)

started :: Task -> Bool
started task = status task `elem` [Progress, Cancelled, Done]

takenOver :: Task -> Bool
takenOver task = status task `elem` [Pending, Progress]

forContext :: Context -> Tasks -> Tasks
forContext c = Map.filter (\t -> context t == c)

exceptContext :: Context -> Tasks -> Tasks
exceptContext c = Map.filter (\t -> context t /= c)

defaultTasks :: Elapsed -> Tasks
defaultTasks currentTime =
  Map.fromList
    [ (1, Task Done "Install t" currentTime "inbox")
    , (2, Task Pending "Learn how to use t" currentTime "inbox")
    , (3, Task Pending "Finally fix issue T#2345" currentTime "work")
    ]

totalCount :: Map.HashMap k v -> Int
totalCount = Map.size

countByStatus :: Status -> Tasks -> Int
countByStatus s = Map.foldl' operator 0
  where
    operator count Task {status = taskStatus}
      | taskStatus == s = count + 1
      | otherwise = count

groupByContext :: Tasks -> Map.HashMap Context Tasks
groupByContext = Map.foldlWithKey' mergeContexts Map.empty
  where
    mergeContexts contexts taskId task =
      Map.alter (mergeTasks taskId task) (context task) contexts
    mergeTasks taskId task maybeOtherTasks =
      case maybeOtherTasks of
        Just otherTasks -> Just (Map.insert taskId task otherTasks)
        Nothing -> Just (Map.fromList [(taskId, task)])

refs :: Task -> [Ref]
refs = extractRefs . text
