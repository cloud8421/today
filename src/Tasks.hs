{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Tasks where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, ask)
import Data.Aeson
import Data.Hourglass (timeDiff)
import Data.Hourglass.Types.Orphans ()
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, member)
import Data.Text (Text)
import GHC.Generics
import Refs (Ref, extractRefs)
import Safe (maximumDef)
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

type Tasks = Map TaskId Task

data ContextFilter
  = All
  | Include Tasks.Context
  | Exclude Tasks.Context

defaultContext :: Context
defaultContext = "inbox"

newTaskId :: Tasks -> TaskId
newTaskId = (+) 1 . maximumDef 0 . Map.keys

add :: MonadReader Elapsed m => Text -> Context -> Tasks -> m Tasks
add taskText taskContext tasks = do
  currentTime <- ask
  let taskId = newTaskId tasks
      task = Task Pending taskText currentTime taskContext
  pure (Map.insert taskId task tasks)

remove :: Tasks -> TaskId -> Tasks
remove = flip Map.delete

clearCompleted :: Tasks -> Tasks
clearCompleted = Map.filter (\t -> status t `elem` [Pending, Progress])

updateTask ::
     (MonadError String m, MonadReader Elapsed m)
  => (Task -> Task)
  -> TaskId
  -> Tasks
  -> m Tasks
updateTask updateFn taskId tasks
  | taskId `member` tasks = do
    currentTime <- ask
    pure
      (Map.adjust (\t -> (updateFn t) {lastUpdate = currentTime}) taskId tasks)
  | otherwise = throwError "Task not found"

updateStatus ::
     (MonadError String m, MonadReader Elapsed m)
  => Status
  -> TaskId
  -> Tasks
  -> m Tasks
updateStatus newStatus = updateTask (\t -> t {status = newStatus})

updateText ::
     (MonadError String m, MonadReader Elapsed m)
  => Text
  -> TaskId
  -> Tasks
  -> m Tasks
updateText newText = updateTask (\t -> t {text = newText})

updateContext ::
     (MonadError String m, MonadReader Elapsed m)
  => Context
  -> TaskId
  -> Tasks
  -> m Tasks
updateContext newContext = updateTask (\t -> t {context = newContext})

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
  Map.fromList $
  zip
    [1 ..]
    [ Task Done "Install today" currentTime "inbox"
    , Task Pending "Learn how to use today" currentTime "inbox"
    , Task Pending "Finally fix issue TODAY#2345" currentTime "work"
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
