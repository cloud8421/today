{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (join)
import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B
import Data.Semigroup ((<>))
import Data.Text as T
import qualified Data.Text.Lazy.IO as I
import Lib
import Options.Applicative
import System.Directory
import qualified Ui

data Opts =
  Opts
    { taskFilePath :: FilePath
    , subCommand :: SubCommand
    }

data SubCommand
  = CreateTask [Text]
  | ListTasks
  | DeleteTask Int

defaultTaskFilePath :: FilePath
defaultTaskFilePath = "./tasks.json"

createTaskFile :: FilePath -> Tasks -> IO ()
createTaskFile path tasks = I.writeFile path (encodeToLazyText tasks)

ensureTaskFile :: FilePath -> Tasks -> IO ()
ensureTaskFile path tasks =
  ifM (doesFileExist path) (return ()) (createTaskFile path tasks)

loadTasksFromFile :: FilePath -> IO (Either String Tasks)
loadTasksFromFile path = eitherDecode <$> B.readFile path

listTasks :: FilePath -> IO ()
listTasks taskFilePath = do
  result <- loadTasksFromFile taskFilePath
  case result of
    Left err -> Ui.displayError err
    Right tasks -> Ui.displayTasks tasks

createTask :: Text -> FilePath -> IO ()
createTask text taskFilePath = do
  result <- loadTasksFromFile taskFilePath
  case result of
    Left err -> Ui.displayError err
    Right tasks -> do
      createTaskFile taskFilePath newTasks
      Ui.displayTasks newTasks
      where newTasks = addTask tasks text

deleteTask :: Int -> FilePath -> IO ()
deleteTask taskId taskFilePath = do
  result <- loadTasksFromFile taskFilePath
  case result of
    Left err -> Ui.displayError err
    Right tasks -> do
      createTaskFile taskFilePath newTasks
      Ui.displayTasks newTasks
      where newTasks = removeTask tasks taskId

textArgument :: Mod ArgumentFields String -> Parser Text
textArgument = fmap pack . strArgument

taskFilePathOption :: Parser FilePath
taskFilePathOption =
  strOption
    (long "taskfile" <> short 'f' <> metavar "TASKFILE" <>
     value defaultTaskFilePath <>
     showDefault <>
     help "Which taskfile to use")

optsParser :: ParserInfo Opts
optsParser = info (helper <*> programOptions) description
  where
    description :: InfoMod Opts
    description =
      fullDesc <> progDesc "T - CLI task manager" <>
      header "T - CLI task manager"
    programOptions :: Parser Opts
    programOptions =
      Opts <$> taskFilePathOption <*>
      hsubparser (listTasksCommand <> createTaskCommand <> deleteTaskCommand)
    listTasksCommand :: Mod CommandFields SubCommand
    listTasksCommand =
      command "list" (info (pure ListTasks) (progDesc "List current tasks"))
    createTaskCommand :: Mod CommandFields SubCommand
    createTaskCommand =
      command "create" (info createOptions (progDesc "Create a new task"))
    createOptions :: Parser SubCommand
    createOptions =
      CreateTask <$> many (textArgument (help "Text of the new task"))
    deleteTaskCommand :: Mod CommandFields SubCommand
    deleteTaskCommand =
      command "delete" (info deleteOptions (progDesc "Delete an existing task"))
    deleteOptions :: Parser SubCommand
    deleteOptions =
      DeleteTask <$> argument auto (help "ID of the task to delete")

main :: IO ()
main = do
  (opts :: Opts) <- execParser optsParser
  ensureTaskFile (taskFilePath opts) defaultTasks
  case subCommand opts of
    CreateTask textFrags -> createTask text (taskFilePath opts)
      where text = T.intercalate " " textFrags
    ListTasks -> listTasks (taskFilePath opts)
    DeleteTask id -> deleteTask id (taskFilePath opts)
