{-# LANGUAGE OverloadedStrings #-}

module Ui where

import qualified Data.Map.Strict as Map
import qualified Data.Sort as Sort
import qualified Data.Text as Text
import qualified Format
import Options.Applicative.Help.Pretty
import qualified Refs
import qualified Taskfile
import qualified Tasks
import Time.Types (Elapsed, Seconds)

data RefItem =
  RefItem Refs.Service Refs.UrlTemplate

newtype RefList =
  RefList [RefItem]

type GroupName = String

data TaskGroup =
  TaskGroup GroupName Tasks.Tasks Elapsed Refs.RefMap

newtype Stats =
  Stats Tasks.Tasks

data TaskList =
  TaskList [TaskGroup] Stats

data TodayScope
  = Beginning
  | End

data Today =
  Today TodayScope Tasks.Tasks Refs.RefMap

newtype StatusIcon =
  StatusIcon Tasks.Status

newtype StatusLabel =
  StatusLabel Tasks.Status

data TaskText =
  TaskText Tasks.Status String

data TaskRefs =
  TaskRefs Tasks.Task Refs.RefMap

class Render a where
  render :: a -> Doc

instance Render RefItem where
  render (RefItem service urlTemplate) =
    text (Text.unpack service) <+> "->" <+> text (Text.unpack urlTemplate)

instance Render RefList where
  render (RefList []) = "No refk lookup rules setup in the current Taskfile"
  render (RefList someRefItems) = fillSep $ map render someRefItems

instance Render Seconds where
  render seconds = black $ text $ humanDuration seconds
    where
      secondsInOneHour = 60 * 60
      secondsInOneDay = secondsInOneHour * 24
      humanDuration s
        | s < 60 = show s
        | s >= 60 && s < secondsInOneHour = Format.minute (fromEnum (div s 60))
        | s >= secondsInOneHour && s < secondsInOneDay =
          Format.hour (fromEnum (div s secondsInOneHour))
        | otherwise = Format.day (fromEnum (div s secondsInOneDay))

instance Render StatusIcon where
  render (StatusIcon Tasks.Done) = green "✔ "
  render (StatusIcon Tasks.Pending) = magenta "◻ "
  render (StatusIcon Tasks.Progress) = "… "
  render (StatusIcon Tasks.Cancelled) = red "✖ "

instance Render StatusLabel where
  render (StatusLabel Tasks.Done) = ":white_check_mark:"
  render (StatusLabel Tasks.Pending) = ":hourglass:"
  render (StatusLabel Tasks.Progress) = ":spinner:"
  render (StatusLabel Tasks.Cancelled) = ":x:"

instance Render TaskText where
  render (TaskText Tasks.Done taskText) = black $ text taskText
  render (TaskText Tasks.Pending taskText) = white $ text taskText
  render (TaskText Tasks.Progress taskText) = white $ text taskText
  render (TaskText Tasks.Cancelled taskText) = black $ text taskText

instance Render TaskRefs where
  render (TaskRefs task refMap) =
    case Tasks.refs task of
      [] -> empty
      someRefs -> hardline <+> align (vsep (map taskRefItem someRefs))
    where
      taskRefItem ref =
        "|" <+>
        text (Text.unpack $ Refs.refId ref) <+>
        ":" <+> text (Text.unpack $ Refs.resolveRef ref refMap)

instance Render Stats where
  render (Stats tasks) =
    black (text (Format.taskPercentDone percentDone)) <$$>
    green (text (Format.groupCount doneCount)) <+>
    black "done ·" <+>
    text (Format.groupCount progressCount) <+>
    black "in progress ·" <+>
    magenta (text (Format.groupCount pendingCount)) <+>
    black "pending ·" <+>
    red (text (Format.groupCount cancelledCount)) <+> black "cancelled"
    where
      doneCount = Tasks.countByStatus Tasks.Done tasks
      progressCount = Tasks.countByStatus Tasks.Progress tasks
      pendingCount = Tasks.countByStatus Tasks.Pending tasks
      cancelledCount = Tasks.countByStatus Tasks.Cancelled tasks
      percentDone :: Float
      percentDone =
        case Map.size tasks of
          0 -> 0
          otherCount -> 100 * fromIntegral doneCount / fromIntegral otherCount

instance Render TaskGroup where
  render (TaskGroup groupName groupTasks currentTime refMap) =
    prefixedGroupName <+> groupCount <$$> groupTaskList <+> hardline
    where
      prefixedGroupName = underline $ text $ "@" ++ groupName
      groupCount =
        black $
        text $
        Format.groupCountOverTotal
          (Tasks.countByStatus Tasks.Done groupTasks)
          (Tasks.totalCount groupTasks)
      orderedTasks =
        Sort.sortOn
          (\(_taskId, task) -> -Tasks.lastUpdate task)
          (Map.toList groupTasks)
      taskItem (taskId, task) =
        let Tasks.Task {Tasks.status = taskStatus, Tasks.text = taskText} = task
            taskAndAge =
              render (TaskText taskStatus (Text.unpack taskText)) <+>
              render (Tasks.age task currentTime)
         in black $
            text (Format.paddedTaskId taskId) <+>
            render (StatusIcon taskStatus) <+>
            align (taskAndAge <> render (TaskRefs task refMap))
      groupTaskList = vsep $ map taskItem orderedTasks

instance Render TaskList where
  render (TaskList [] _stats) =
    "No tasks in your list. Enjoy some free time!" <+>
    hardline <+> "You can add a new task with 't add Buy milk'"
  render (TaskList tgs tls) = vsep (map render tgs) <$$> render tls

instance Render TodayScope where
  render Beginning = "*Today:*"
  render End = "*Out for today:*"

instance Render Today where
  render (Today scope tasks refMap)
    | Map.null tasks = "No tasks available"
    | otherwise =
      render scope <$$> hardline <> vsep (map todayLine (Map.toList tasks))
    where
      todayLine (_id, t) =
        "•" <+>
        render (StatusLabel (Tasks.status t)) <+>
        text (Text.unpack $ Refs.replaceRefs (Tasks.text t) refMap) <> hardline

refList :: Refs.RefMap -> Doc
refList refMap = hardline <+> indent 1 content <+> hardline
  where
    content = render $ RefList $ map (uncurry RefItem) (Map.toList refMap)

taskList :: Tasks.ContextFilter -> Taskfile.Taskfile -> Elapsed -> Doc
taskList contextFilter taskfile currentTime =
  hardline <+> indent 1 content <$$> hardline
  where
    contextTasks =
      Map.filter (Tasks.inContext contextFilter) (Taskfile.tasks taskfile)
    tg (c, ts) =
      TaskGroup (Text.unpack c) ts currentTime (Taskfile.refs taskfile)
    tgs = map tg $ Map.toList $ Tasks.groupByContext contextTasks
    content = render $ TaskList tgs (Stats contextTasks)

today :: Tasks.ContextFilter -> Taskfile.Taskfile -> Doc
today contextFilter taskfile = content <+> hardline
  where
    content = render (Today Beginning todayTasks (Taskfile.refs taskfile))
    todayTask t = Tasks.takenOver t && Tasks.inContext contextFilter t
    todayTasks = Map.filter todayTask (Taskfile.tasks taskfile)

outForToday :: Tasks.ContextFilter -> Taskfile.Taskfile -> Doc
outForToday contextFilter taskfile = content <+> hardline
  where
    content = render (Today End todayTasks (Taskfile.refs taskfile))
    todayTask t = Tasks.started t && Tasks.inContext contextFilter t
    todayTasks = Map.filter todayTask (Taskfile.tasks taskfile)

error :: String -> Doc
error err = hardline <+> red (text err) <+> hardline
