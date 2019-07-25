{-# LANGUAGE OverloadedStrings #-}

module Ui
  ( render
  , displayError
  ) where

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

formatContext :: T.Text -> T.Text
formatContext = T.cons '@'

displayGroupHeader :: T.Text -> Tasks -> IO ()
displayGroupHeader context tasks = do
  setSGR [SetColor Foreground Vivid White]
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
  setSGR [SetColor Foreground Vivid Green]
  TIO.putStr (padLeft (T.pack (printf "%d " (countByStatus Done tasks))))
  setSGR [SetColor Foreground Vivid Black]
  TIO.putStr "done · "
  setSGR [SetColor Foreground Vivid White]
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
      setSGR [SetColor Foreground Vivid White]
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
      setSGR [SetColor Foreground Vivid White]
      TIO.putStr text
    Progress -> do
      setSGR [SetColor Foreground Vivid White]
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

render :: Tasks -> Elapsed -> IO ()
render tasks currentTime = do
  spacer
  displayTaskGroups tasks currentTime
  displayStats tasks
  spacer

displayError :: String -> IO ()
displayError err = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn err
  setSGR [Reset]
