{-# LANGUAGE OverloadedStrings #-}

module Ui
  ( displayTasks
  , displayError
  ) where

import Data.Text
import qualified Data.Text.IO as T
import System.Console.ANSI
import Tasks
import Text.Printf

spacer :: IO ()
spacer = T.putStrLn ""

padLeft :: Text -> Text
padLeft = append "  "

displayInboxHeader :: Tasks -> IO ()
displayInboxHeader tasks = do
  setSGR [SetColor Foreground Vivid White]
  T.putStr (padLeft "Inbox ")
  setSGR [SetColor Foreground Vivid Black]
  putStrLn inboxCount
  setSGR [Reset]
  where
    inboxCount = printf "[%d/%d]" (countByStatus Done tasks) (totalCount tasks)

displayInboxTasks :: Tasks -> IO ()
displayInboxTasks tasks = mapM_ displayTask (toList tasks)
  where
    displayTask (id, task) = do
      setSGR [SetColor Foreground Vivid Black]
      T.putStr (padLeft (padLeft (pack (printf "%d." id))))
      T.putStr " "
      displayStatus (status task)
      T.putStr "  "
      setSGR [SetColor Foreground Vivid White]
      T.putStrLn (text task)

displayStats :: Tasks -> IO ()
displayStats tasks = do
  setSGR [SetColor Foreground Vivid Green]
  T.putStr (padLeft (pack (printf "%d " (countByStatus Done tasks))))
  setSGR [SetColor Foreground Vivid Black]
  T.putStr "done · "
  setSGR [SetColor Foreground Vivid White]
  T.putStr (pack (printf "%d " (countByStatus Progress tasks)))
  setSGR [SetColor Foreground Vivid Black]
  T.putStr "in progress · "
  setSGR [SetColor Foreground Vivid Magenta]
  T.putStr (pack (printf "%d " (countByStatus Pending tasks)))
  setSGR [SetColor Foreground Vivid Black]
  T.putStr "pending · "
  setSGR [SetColor Foreground Vivid Red]
  T.putStr (pack (printf "%d " (countByStatus Cancelled tasks)))
  setSGR [SetColor Foreground Vivid Black]
  T.putStr "cancelled"
  spacer

displayStatus :: Status -> IO ()
displayStatus status =
  case status of
    Done -> do
      setSGR [SetColor Foreground Vivid Green]
      T.putStr "✔"
    Pending -> do
      setSGR [SetColor Foreground Vivid Magenta]
      T.putStr "◻"
    Progress -> do
      setSGR [SetColor Foreground Vivid White]
      T.putStr "…"
    Cancelled -> do
      setSGR [SetColor Foreground Vivid Red]
      T.putStr "✖"

displayTasks :: Tasks -> IO ()
displayTasks tasks = do
  spacer
  displayInboxHeader tasks
  displayInboxTasks tasks
  spacer
  displayStats tasks
  spacer

displayError :: String -> IO ()
displayError err = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn err
  setSGR [Reset]
