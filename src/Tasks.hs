{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Tasks where

import Data.Aeson
import qualified Data.HashMap.Strict as Map
import Data.Hourglass (timeDiff)
import Data.Hourglass.Types.Orphans
import qualified Data.List as L
import Data.Maybe
import Data.Text
import GHC.Generics
import Refs
import Text.Regex.PCRE
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
remove tasks taskId = Map.delete taskId tasks

clearCompleted :: Tasks -> Tasks
clearCompleted = Map.filter (\t -> status t `elem` [Pending, Progress])

updateStatus :: Status -> Tasks -> TaskId -> Elapsed -> Either String Tasks
updateStatus newStatus tasks taskId currentTime =
  case Map.lookup taskId tasks of
    Nothing -> Left "Task not found"
    Just task ->
      Right
        (Map.adjust
           (\t -> t {status = newStatus, lastUpdate = currentTime})
           taskId
           tasks)

updateText :: Text -> Tasks -> TaskId -> Elapsed -> Either String Tasks
updateText newText tasks taskId currentTime =
  case Map.lookup taskId tasks of
    Nothing -> Left "Task not found"
    Just task ->
      Right
        (Map.adjust
           (\t -> t {text = newText, lastUpdate = currentTime})
           taskId
           tasks)

updateContext :: Context -> Tasks -> TaskId -> Elapsed -> Either String Tasks
updateContext newContext tasks taskId currentTime =
  case Map.lookup taskId tasks of
    Nothing -> Left "Task not found"
    Just task ->
      Right
        (Map.adjust
           (\t -> t {context = newContext, lastUpdate = currentTime})
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

totalCount :: Map.HashMap k v -> Int
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

refs :: Task -> [Ref]
refs task = L.map (\[s, issueNo] -> Ref s issueNo) matches
  where
    result :: AllTextMatches [] String
    result = unpack (text task) =~ refMatcher
    rawMatches = getAllTextMatches result
    matches = L.map (splitOn "#" . pack) rawMatches

expandRefs :: Task -> RefMap -> Text
expandRefs task refMap = L.foldl expandRef (text task) matches
  where
    result :: AllTextMatches [] String
    result = unpack (text task) =~ refMatcher
    rawMatches :: [String]
    rawMatches = getAllTextMatches result
    matches :: [Text]
    matches = L.map pack rawMatches
    expandRef t match =
      let ref = Ref s issueNo
          [s, issueNo] = splitOn "#" match
       in case Map.lookup (repo ref) refMap of
            Just repoPath -> replace match (buildRefUrl ref repoPath) t
            Nothing -> t

toListWithId :: Map.HashMap k v -> [(k, v)]
toListWithId = Map.toList

toList :: Map.HashMap k v -> [v]
toList = Map.elems
