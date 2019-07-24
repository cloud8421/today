module Ui
  ( displayTasks
  , displayError
  ) where

import Lib (Tasks)
import System.Console.ANSI

displayTasks :: Tasks -> IO ()
displayTasks tasks = do
  setSGR [SetColor Foreground Vivid White]
  print tasks
  setSGR [Reset]

displayError :: String -> IO ()
displayError err = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn err
  setSGR [Reset]
