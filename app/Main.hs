{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (join)
import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B
import Data.Semigroup ((<>))
import qualified Data.Text.Lazy.IO as I
import Lib
import Options.Applicative
import System.Directory
import qualified Ui

newtype Config =
  Config
    { taskFilePath :: FilePath
    }

defaultTaskFilePath :: FilePath
defaultTaskFilePath = "./tasks.json"

createTaskFile :: FilePath -> Tasks -> IO ()
createTaskFile path tasks = I.writeFile path (encodeToLazyText tasks)

ensureTaskFile :: FilePath -> Tasks -> IO ()
ensureTaskFile path tasks =
  ifM (doesFileExist path) (return ()) (createTaskFile path tasks)

loadTasksFromFile :: FilePath -> IO (Either String Tasks)
loadTasksFromFile path = eitherDecode <$> B.readFile path

taskFilePathOption :: Parser FilePath
taskFilePathOption =
  strOption
    (long "taskfile" <> short 'f' <> metavar "TASKFILE" <>
     value defaultTaskFilePath <>
     showDefault <>
     help "Which taskfile to use")

parser :: Parser (IO ())
parser = hsubparser (listTasksCommand <> createTaskCommand)
  where
    listTasksCommand =
      command "list" (info (listTasks <$> configParser) listTasksDesc)
    createTaskCommand =
      command "create" (info (createTask <$> configParser) createTaskDesc)
    configParser = Config <$> taskFilePathOption
    listTasksDesc = progDesc "List all available tasks"
    createTaskDesc = progDesc "Create a new task"

listTasks :: Config -> IO ()
listTasks config = do
  ensureTaskFile (taskFilePath config) defaultTasks
  result <- loadTasksFromFile (taskFilePath config)
  case result of
    Left err -> Ui.displayError err
    Right tasks -> Ui.displayTasks tasks

createTask :: Config -> IO ()
createTask config = do
  ensureTaskFile (taskFilePath config) defaultTasks
  result <- loadTasksFromFile (taskFilePath config)
  case result of
    Left err -> Ui.displayError err
    Right tasks -> do
      createTaskFile (taskFilePath config) newTasks
      Ui.displayTasks newTasks
      where newTasks = addTask tasks "My new task"

main :: IO ()
main = join $ execParser opts
  where
    opts = info (parser <**> helper) desc
    desc = fullDesc <> progDesc "CLI based task manager" <> header "T"
