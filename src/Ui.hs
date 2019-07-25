{-# LANGUAGE OverloadedStrings #-}

module Ui
  ( displayTasks
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

displayInboxHeader :: Tasks -> IO ()
displayInboxHeader tasks = do
  setSGR [SetColor Foreground Vivid White]
  TIO.putStr (padLeft "Inbox ")
  setSGR [SetColor Foreground Vivid Black]
  putStrLn inboxCount
  setSGR [Reset]
  where
    inboxCount = printf "[%d/%d]" (countByStatus Done tasks) (totalCount tasks)

displayInboxTasks :: Tasks -> Elapsed -> IO ()
displayInboxTasks tasks currentTime = mapM_ displayTask (toList tasks)
  where
    displayTask (id, task) = do
      setSGR [SetColor Foreground Vivid Black]
      TIO.putStr (padLeft (padLeft (T.pack (printf "%d." id))))
      TIO.putStr " "
      displayStatus (status task)
      TIO.putStr "  "
      setSGR [SetColor Foreground Vivid White]
      TIO.putStr (text task)
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

displayTasks :: Tasks -> Elapsed -> IO ()
displayTasks tasks currentTime = do
  spacer
  displayInboxHeader tasks
  displayInboxTasks tasks currentTime
  spacer
  displayStats tasks
  spacer

displayError :: String -> IO ()
displayError err = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn err
  setSGR [Reset]
