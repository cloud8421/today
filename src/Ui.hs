{-# LANGUAGE OverloadedStrings #-}

module Ui
  ( render
  , showToday
  , showTodayByContext
  , displayError
  ) where

import qualified Data.List as L
import qualified Data.Sort as Sort
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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

displayGroupHeader :: Context -> Tasks -> IO ()
displayGroupHeader context tasks = do
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

displayTaskRefs :: Task -> RefMap -> IO ()
displayTaskRefs task refMap = mapM_ displayTaskRef (refs task)
  where
    displayTaskRef ref = do
      TIO.putStr "          • "
      TIO.putStrLn (resolveRef ref refMap)

displayTasks :: Tasks -> RefMap -> Elapsed -> IO ()
displayTasks tasks refMap currentTime = mapM_ displayTask orderedTasks
  where
    orderedTasks =
      Sort.sortOn (\(id, task) -> -lastUpdate task) (toListWithId tasks)
    displayTask (id, task) = do
      setSGR [SetColor Foreground Vivid Black]
      TIO.putStr (T.pack (printf "%5d." id))
      TIO.putStr " "
      displayStatus (status task)
      TIO.putStr "  "
      displayText (status task) (text task)
      TIO.putStr " "
      setSGR [SetColor Foreground Vivid Black]
      TIO.putStrLn (formatSeconds (age task currentTime))
      displayTaskRefs task refMap

displayStats :: Tasks -> IO ()
displayStats tasks = do
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

displayStatus :: Status -> IO ()
displayStatus status =
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

displayText :: Status -> T.Text -> IO ()
displayText status text =
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

displayTaskGroups :: Taskfile.Taskfile -> Elapsed -> IO ()
displayTaskGroups taskfile currentTime =
  mapM_ displayGroup (toListWithId taskGroups)
  where
    taskGroups = groupByContext (Taskfile.tasks taskfile)
    displayGroup (context, groupTasks) = do
      displayGroupHeader context groupTasks
      displayTasks groupTasks (Taskfile.refs taskfile) currentTime
      spacer

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
      TIO.putStrLn (expandRefs task refMap)

showToday :: Taskfile.Taskfile -> IO ()
showToday taskfile = todayList todayTasks (Taskfile.refs taskfile)
  where
    todayTasks = L.filter started (toList (Taskfile.tasks taskfile))

showTodayByContext :: Context -> Taskfile.Taskfile -> IO ()
showTodayByContext c taskfile = todayList todayTasks (Taskfile.refs taskfile)
  where
    taskForToday task = context task == c && started task
    todayTasks = L.filter taskForToday (toList (Taskfile.tasks taskfile))

displayEmpty :: IO ()
displayEmpty = do
  TIO.putStrLn (padLeft "No tasks in your list. Enjoy some free time!")
  spacer
  TIO.putStrLn (padLeft "You can add a new task with 't add Buy milk'")

render :: Taskfile.Taskfile -> Elapsed -> IO ()
render taskfile currentTime = do
  spacer
  body
  spacer
  where
    body =
      case totalCount (Taskfile.tasks taskfile) of
        0 -> displayEmpty
        other -> do
          displayTaskGroups taskfile currentTime
          displayStats (Taskfile.tasks taskfile)

displayError :: String -> IO ()
displayError err = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn err
  setSGR [Reset]
