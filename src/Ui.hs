{-# LANGUAGE OverloadedStrings #-}

module Ui
  ( displayTasks
  , displayError
  ) where

import Data.Text
import qualified Data.Text.IO as T
import Lib
import System.Console.ANSI
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
    inboxCount = printf "[%d/%d]" (doneCount tasks) (totalCount tasks)

displayInboxTasks :: Tasks -> IO ()
displayInboxTasks tasks = mapM_ displayTask (toList tasks)
  where
    displayTask (id, task) = do
      setSGR [SetColor Foreground Vivid White]
      T.putStr (padLeft (pack (printf "%d. " id)))
      displayStatus (status task)
      T.putStr " "
      setSGR [SetColor Foreground Vivid White]
      T.putStrLn (text task)

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
  spacer
  displayInboxTasks tasks
  spacer

displayError :: String -> IO ()
displayError err = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn err
  setSGR [Reset]
