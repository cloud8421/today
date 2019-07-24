module Ui
  ( displayTasks
  , displayError
  ) where

import Lib
import System.Console.ANSI
import Text.Printf

spacer :: IO ()
spacer = putStrLn ""

padLeft :: String -> String
padLeft str = "  " ++ str

displayInboxHeader :: Tasks -> IO ()
displayInboxHeader tasks = do
  setSGR [SetColor Foreground Vivid White]
  putStr (padLeft "Inbox ")
  setSGR [SetColor Foreground Vivid Black]
  putStrLn inboxCount
  setSGR [Reset]
  where
    inboxCount = printf "[%d/%d]" (doneCount tasks) (totalCount tasks)

displayTasks :: Tasks -> IO ()
displayTasks tasks = do
  spacer
  displayInboxHeader tasks
  spacer

displayError :: String -> IO ()
displayError err = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn err
  setSGR [Reset]
