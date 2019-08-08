{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Tasks where

import Control.Monad.Except (MonadError, throwError)
import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, member)
import Data.Hourglass (timeDiff)
import Data.Hourglass.Types.Orphans
import Data.Text (Text)
import GHC.Generics
import Refs (Ref, extractRefs)
import Time.Types (Elapsed, Seconds)
import Safe (maximumDef)

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

type Tasks = Map TaskId Task

data ContextFilter
  = All
  | Include Tasks.Context
  | Exclude Tasks.Context

defaultContext :: Context
defaultContext = "inbox"

newTaskId :: Tasks -> TaskId
newTaskId = (+) 1 . maximumDef 0 . Map.keys

add :: Text -> Elapsed -> Context -> Tasks -> Tasks
add text currentTime context tasks =
  let taskId = newTaskId tasks
      task = Task Pending text currentTime context
   in Map.insert taskId task tasks

remove :: Tasks -> TaskId -> Tasks
remove = flip Map.delete

clearCompleted :: Tasks -> Tasks
clearCompleted = Map.filter (\t -> status t `elem` [Pending, Progress])

updateTask :: MonadError String m => (Task -> Task) -> TaskId -> Tasks -> m Tasks
updateTask updateFn taskId tasks
  | taskId `member` tasks = pure (Map.adjust updateFn taskId tasks)
  | otherwise = throwError "Task not found"

updateStatus :: MonadError String m => Status -> Elapsed -> TaskId -> Tasks -> m Tasks
updateStatus newStatus currentTime =
  updateTask (\t -> t {status = newStatus, lastUpdate = currentTime})

updateText :: MonadError String m => Text -> Elapsed -> TaskId -> Tasks -> m Tasks
updateText newText currentTime =
  updateTask (\t -> t {text = newText, lastUpdate = currentTime})

updateContext :: MonadError String m => Context -> Elapsed -> TaskId -> Tasks -> m Tasks
updateContext newContext currentTime =
  updateTask (\t -> t {context = newContext, lastUpdate = currentTime})

age :: Task -> Elapsed -> Seconds
age task currentTime = timeDiff currentTime (lastUpdate task)

started :: Task -> Bool
started task = status task `elem` [Progress, Cancelled, Done]

takenOver :: Task -> Bool
takenOver task = status task `elem` [Pending, Progress]

inContext :: ContextFilter -> Task -> Bool
inContext All _task = True
inContext (Include c) task = context task == c
inContext (Exclude c) task = context task /= c

defaultTasks :: Elapsed -> Tasks
defaultTasks currentTime =
  Map.fromList
  $ zip [1..]
    [ Task Done "Install t" currentTime "inbox"
    , Task Pending "Learn how to use t" currentTime "inbox"
    , Task Pending "Finally fix issue T#2345" currentTime "work"
    ]

totalCount :: Map k v -> Int
totalCount = Map.size

countByStatus :: Status -> Tasks -> Int
countByStatus s = Map.foldl' operator 0
  where
    operator count Task {status = taskStatus}
      | taskStatus == s = count + 1
      | otherwise = count

groupByContext :: Tasks -> Map Context Tasks
groupByContext = Map.foldlWithKey' mergeContexts mempty
  where
    mergeContexts contexts taskId task =
      Map.alter (mergeTasks taskId task) (context task) contexts
    mergeTasks taskId task (Just otherTasks) =
      Just (Map.insert taskId task otherTasks)
    mergeTasks taskId task Nothing = Just (Map.fromList [(taskId, task)])

refs :: Task -> [Ref]
refs = extractRefs . text
