{-# LANGUAGE OverloadedStrings #-}

module Ui
  ( render
  , showToday
  , displayError
  ) where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Console.ANSI
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
  TIO.putStr (padLeft (formatContext context))
  TIO.putStr " "
  setSGR [SetColor Foreground Vivid Black]
  putStrLn inboxCount
  setSGR [Reset]
  where
    inboxCount = printf "[%d/%d]" (countByStatus Done tasks) (totalCount tasks)

displayTasks :: Tasks -> Elapsed -> IO ()
displayTasks tasks currentTime = mapM_ displayTask (toList tasks)
  where
    displayTask (id, task) = do
      setSGR [SetColor Foreground Vivid Black]
      TIO.putStr (padLeft (padLeft (T.pack (printf "%d." id))))
      TIO.putStr " "
      displayStatus (status task)
      TIO.putStr "  "
      displayText (status task) (text task)
      TIO.putStr " "
      setSGR [SetColor Foreground Vivid Black]
      TIO.putStrLn (formatSeconds (age task currentTime))

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

displayTaskGroups :: Tasks -> Elapsed -> IO ()
displayTaskGroups tasks currentTime = mapM_ displayGroup (toList taskGroups)
  where
    taskGroups = groupByContext tasks
    displayGroup (context, groupTasks) = do
      displayGroupHeader context groupTasks
      displayTasks groupTasks currentTime
      spacer

showToday :: Context -> Tasks -> IO ()
showToday c tasks = do
  spacer
  TIO.putStrLn "*Today:*"
  mapM_ taskLine contextTasks
  spacer
  where
    taskLine (id, task) = do
      TIO.putStr "• "
      TIO.putStrLn (text task)
    taskForToday (id, task) = context task == c && started task
    contextTasks = L.filter taskForToday (toList tasks)

displayEmpty :: IO ()
displayEmpty = do
  TIO.putStrLn (padLeft "No tasks in your list. Enjoy some free time!")
  spacer
  TIO.putStrLn (padLeft "You can add a new task with 't add Buy milk'")

render :: Tasks -> Elapsed -> IO ()
render tasks currentTime = do
  spacer
  body
  spacer
  where
    body =
      case totalCount tasks of
        0 -> displayEmpty
        other -> do
          displayTaskGroups tasks currentTime
          displayStats tasks

displayError :: String -> IO ()
displayError err = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn err
  setSGR [Reset]
