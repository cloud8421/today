{-# LANGUAGE OverloadedStrings #-}

module Ui
  ( showTasks
  , showToday
  , showRefs
  , showError
  ) where

import qualified Data.HashMap.Strict as Map
import qualified Data.List as L
import qualified Data.Sort as Sort
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Refs
import System.Console.ANSI
import qualified Taskfile
import Tasks
import Text.Printf
import Time.Types (Elapsed, Seconds)

spacer :: IO ()
spacer = TIO.putStrLn ""

padLeft :: T.Text -> T.Text
padLeft = T.append "  "

formatSeconds :: Seconds -> T.Text
formatSeconds seconds = T.pack (humanDuration seconds)
  where
    secondsInOneHour = 60 * 60
    secondsInOneDay = secondsInOneHour * 24
    humanDuration s
      | s < 60 = show s
      | s >= 60 && s < secondsInOneHour = printf "%dm" (fromEnum (div s 60))
      | s >= secondsInOneHour && s < secondsInOneDay =
        printf "%dh" (fromEnum (div s secondsInOneHour))
      | s >= secondsInOneDay = printf "%dd" (fromEnum (div s secondsInOneDay))

formatContext :: Context -> T.Text
formatContext = T.cons '@'

showGroupHeader :: Context -> Tasks -> IO ()
showGroupHeader context tasks = do
  setSGR [Reset]
  TIO.putStr "  "
  setSGR [Reset, SetUnderlining SingleUnderline]
  TIO.putStr (formatContext context)
  setSGR [Reset, SetColor Foreground Vivid Black]
  TIO.putStr " "
  putStrLn inboxCount
  setSGR [Reset]
  where
    inboxCount = printf "[%d/%d]" (countByStatus Done tasks) (totalCount tasks)

showTaskRefs :: Task -> RefMap -> IO ()
showTaskRefs task refMap = mapM_ showTaskRef (refs task)
  where
    showTaskRef ref = do
      TIO.putStr "          | "
      TIO.putStr (refId ref)
      TIO.putStr ": "
      TIO.putStrLn (resolveRef ref refMap)

showGroupBody :: Tasks -> RefMap -> Elapsed -> IO ()
showGroupBody tasks refMap currentTime = mapM_ showTask orderedTasks
  where
    orderedTasks =
      Sort.sortOn (\(id, task) -> -lastUpdate task) (Map.toList tasks)
    showTask (id, task) = do
      setSGR [SetColor Foreground Vivid Black]
      TIO.putStr (T.pack (printf "%5d." id))
      TIO.putStr " "
      showStatus (status task)
      TIO.putStr "  "
      showText (status task) (text task)
      TIO.putStr " "
      setSGR [SetColor Foreground Vivid Black]
      TIO.putStrLn (formatSeconds (age task currentTime))
      showTaskRefs task refMap

showStats :: Tasks -> IO ()
showStats tasks = do
  setSGR [SetColor Foreground Vivid Black]
  TIO.putStrLn
    (padLeft (T.pack (printf "%0.f%% of all tasks complete." percentDone)))
  setSGR [SetColor Foreground Vivid Green]
  TIO.putStr (padLeft (T.pack (printf "%d " doneCount)))
  setSGR [SetColor Foreground Vivid Black]
  TIO.putStr "done · "
  setSGR [Reset]
  TIO.putStr (T.pack (printf "%d " (countByStatus Progress tasks)))
  setSGR [SetColor Foreground Vivid Black]
  TIO.putStr "in progress · "
  setSGR [SetColor Foreground Vivid Magenta]
  TIO.putStr (T.pack (printf "%d " (countByStatus Pending tasks)))
  setSGR [SetColor Foreground Vivid Black]
  TIO.putStr "pending · "
  setSGR [SetColor Foreground Vivid Red]
  TIO.putStr (T.pack (printf "%d " (countByStatus Cancelled tasks)))
  setSGR [SetColor Foreground Vivid Black]
  TIO.putStr "cancelled"
  spacer
  where
    doneCount = countByStatus Done tasks
    percentDone :: Float
    percentDone =
      case totalCount tasks of
        0 -> 0
        otherCount -> 100 * fromIntegral doneCount / fromIntegral otherCount

showStatus :: Status -> IO ()
showStatus status =
  case status of
    Done -> do
      setSGR [SetColor Foreground Vivid Green]
      TIO.putStr "✔"
    Pending -> do
      setSGR [SetColor Foreground Vivid Magenta]
      TIO.putStr "◻"
    Progress -> do
      setSGR [Reset]
      TIO.putStr "…"
    Cancelled -> do
      setSGR [SetColor Foreground Vivid Red]
      TIO.putStr "✖"

showText :: Status -> T.Text -> IO ()
showText status text =
  case status of
    Done -> do
      setSGR [SetColor Foreground Vivid Black]
      TIO.putStr text
    Pending -> do
      setSGR [Reset]
      TIO.putStr text
    Progress -> do
      setSGR [Reset]
      TIO.putStr text
    Cancelled -> do
      setSGR [SetColor Foreground Vivid Black]
      TIO.putStr text

showTaskGroups :: Tasks.Tasks -> RefMap -> Elapsed -> IO ()
showTaskGroups tasks refs currentTime =
  mapM_ showTaskGroup (Map.toList taskGroups)
  where
    taskGroups = groupByContext tasks
    showTaskGroup (context, groupTasks) = do
      showGroupHeader context groupTasks
      showGroupBody groupTasks refs currentTime
      spacer

statusLabel :: Status -> T.Text
statusLabel Pending = ":hourglass:"
statusLabel Progress = ":spinner:"
statusLabel Done = ":white_check_mark:"
statusLabel Cancelled = ":x:"

todayList :: [Task] -> RefMap -> IO ()
todayList [] refMap = do
  spacer
  TIO.putStrLn (padLeft "No tasks available")
  spacer
todayList tasks refMap = do
  spacer
  TIO.putStrLn "*Today:*"
  mapM_ taskLine tasks
  spacer
  where
    taskLine task = do
      TIO.putStr "• "
      TIO.putStr (statusLabel (status task))
      TIO.putStr " "
      TIO.putStrLn (replaceRefs (text task) refMap)

showToday :: Maybe Context -> Taskfile.Taskfile -> IO ()
showToday Nothing taskfile = todayList todayTasks (Taskfile.refs taskfile)
  where
    todayTasks = L.filter started (Map.elems (Taskfile.tasks taskfile))
showToday (Just c) taskfile = todayList todayTasks (Taskfile.refs taskfile)
  where
    taskForToday task = context task == c && started task
    todayTasks = L.filter taskForToday (Map.elems (Taskfile.tasks taskfile))

showEmpty :: IO ()
showEmpty = do
  TIO.putStrLn (padLeft "No tasks in your list. Enjoy some free time!")
  spacer
  TIO.putStrLn (padLeft "You can add a new task with 't add Buy milk'")

showTasks :: Maybe Context -> Taskfile.Taskfile -> Elapsed -> IO ()
showTasks maybeContext taskfile currentTime = do
  spacer
  body
  spacer
  where
    contextTasks =
      case maybeContext of
        Nothing -> Taskfile.tasks taskfile
        Just c -> Tasks.forContext c (Taskfile.tasks taskfile)
    body =
      case totalCount contextTasks of
        0 -> showEmpty
        other -> do
          showTaskGroups contextTasks (Taskfile.refs taskfile) currentTime
          showStats contextTasks

showRefs :: RefMap -> IO ()
showRefs refMap =
  case totalCount refMap of
    0 -> do
      spacer
      TIO.putStrLn "No ref lookup rules setup in the current Taskfile"
      spacer
    _other -> do
      spacer
      mapM_ showRef (Map.toList refMap)
      spacer
  where
    showRef (repo, repoPath) = do
      TIO.putStr (padLeft repo)
      TIO.putStr " -> "
      TIO.putStrLn repoPath

showError :: String -> IO ()
showError err = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn err
  setSGR [Reset]
