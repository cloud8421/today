{-# LANGUAGE OverloadedStrings #-}

module Ui where

import qualified Data.Map.Strict as Map
import qualified Data.Sort as Sort
import qualified Data.Text as Text
import Options.Applicative.Help.Pretty
import qualified Refs
import qualified Taskfile
import qualified Tasks
import Text.Printf
import Time.Types (Elapsed, Seconds)

data RefItem =
  RefItem
    { service :: String
    , urlTemplate :: String
    }

newtype RefList =
  RefList [RefItem]

data TaskGroup =
  TaskGroup
    { groupName :: String
    , groupTasks :: Tasks.Tasks
    , ct :: Elapsed
    , rm :: Refs.RefMap
    }

newtype Stats =
  Stats Tasks.Tasks

data TaskList =
  TaskList
    { taskGroups :: [TaskGroup]
    , stats :: Stats
    }

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
  render RefItem {service = s, urlTemplate = ut} = text s <+> "->" <+> text ut

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
        | s >= 60 && s < secondsInOneHour = printf "%dm" (fromEnum (div s 60))
        | s >= secondsInOneHour && s < secondsInOneDay =
          printf "%dh" (fromEnum (div s secondsInOneHour))
        | otherwise = printf "%dd" (fromEnum (div s secondsInOneDay))

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
      someRefs -> hardline <+> vsep (map taskRefItem someRefs)
    where
      taskRefItem ref =
        "|" <+>
        text (Text.unpack $ Refs.refId ref) <+>
        ":" <+> text (Text.unpack $ Refs.resolveRef ref refMap)

instance Render Stats where
  render (Stats tasks) =
    black (text (printf "%0.f%% of all tasks complete." percentDone)) <$$>
    green (text (printf "%d" doneCount)) <+>
    black "done ·" <+>
    text (printf "%d" progressCount) <+>
    black "in progress ·" <+>
    magenta (text (printf "%d" pendingCount)) <+>
    black "pending ·" <+>
    red (text (printf "%d" cancelledCount)) <+> black "cancelled"
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
  render TaskGroup { groupName = gn
                   , groupTasks = gts
                   , ct = currentTime
                   , rm = refMap
                   } =
    prefixedGroupName <+> groupCount <$$> groupTaskList <+> hardline
    where
      prefixedGroupName = underline $ text $ "@" ++ gn
      groupCount =
        black $
        text $
        printf
          "[%d/%d]"
          (Tasks.countByStatus Tasks.Done gts)
          (Tasks.totalCount gts)
      orderedTasks =
        Sort.sortOn
          (\(_taskId, task) -> -Tasks.lastUpdate task)
          (Map.toList gts)
      taskItem (taskId, task) =
        let Tasks.Task {Tasks.status = taskStatus, Tasks.text = taskText} = task
            taskAndAge =
              render (TaskText taskStatus (Text.unpack taskText)) <+>
              render (Tasks.age task currentTime)
         in black $
            text (printf "%3d." taskId) <+>
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
    | otherwise = render scope <$$> vsep (map todayLine (Map.toList tasks))
    where
      todayLine (_id, t) =
        "•" <+>
        render (StatusLabel (Tasks.status t)) <+>
        text (Text.unpack $ Refs.replaceRefs (Tasks.text t) refMap)

refList :: Refs.RefMap -> Doc
refList refMap = hardline <+> indent 1 content <+> hardline
  where
    content = render $ RefList $ map toRefItem (Map.toList refMap)
    toRefItem (s, ut) = RefItem (Text.unpack s) (Text.unpack ut)

taskList :: Tasks.ContextFilter -> Taskfile.Taskfile -> Elapsed -> Doc
taskList contextFilter taskfile currentTime =
  hardline <+> indent 1 content <+> hardline
  where
    contextTasks =
      Map.filter (Tasks.inContext contextFilter) (Taskfile.tasks taskfile)
    tg (c, ts) =
      TaskGroup (Text.unpack c) ts currentTime (Taskfile.refs taskfile)
    tgs = map tg $ Map.toList $ Tasks.groupByContext contextTasks
    content = render $ TaskList tgs (Stats contextTasks)

today :: Tasks.ContextFilter -> Taskfile.Taskfile -> Doc
today contextFilter taskfile =
  render (Today Beginning todayTasks (Taskfile.refs taskfile))
  where
    todayTask t = Tasks.takenOver t && Tasks.inContext contextFilter t
    todayTasks = Map.filter todayTask (Taskfile.tasks taskfile)

outForToday :: Tasks.ContextFilter -> Taskfile.Taskfile -> Doc
outForToday contextFilter taskfile =
  render (Today End todayTasks (Taskfile.refs taskfile))
  where
    todayTask t = Tasks.started t && Tasks.inContext contextFilter t
    todayTasks = Map.filter todayTask (Taskfile.tasks taskfile)

error :: String -> Doc
error err = hardline <+> red (text err) <+> hardline
