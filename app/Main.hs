{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Data.Semigroup ((<>))
import Data.Text as T
import Options.Applicative
import System.Hourglass (timeCurrent)
import qualified Taskfile
import qualified Tasks
import Time.Types (Elapsed)
import qualified Ui

data Opts =
  Opts
    { taskFilePath :: FilePath
    , subCommand :: SubCommand
    }

data SubCommand
  = AddTask Text [Text]
  | ListTasks
  | DeleteTask Tasks.TaskId
  | CheckTask Tasks.TaskId
  | CancelTask Tasks.TaskId
  | Update Tasks.TaskId [Text]
  | Today Text

defaultTaskContext :: String
defaultTaskContext = "inbox"

textArgument :: Mod ArgumentFields String -> Parser Text
textArgument = fmap pack . strArgument

textOption :: Mod OptionFields String -> Parser Text
textOption = fmap pack . strOption

taskFilePathOption :: Parser FilePath
taskFilePathOption =
  strOption
    (long "taskfile" <> short 'f' <> metavar "TASKFILE" <>
     value Taskfile.defaultPath <>
     showDefault <>
     help "Which taskfile to use")

taskContextOption :: Parser Text
taskContextOption =
  textOption
    (long "context" <> short 'c' <> value defaultTaskContext <>
     help "The content for the task, e.g. work or home")

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
      hsubparser
        (listTasksCommand <> addTaskCommand <> deleteTaskCommand <>
         checkTaskCommand <>
         cancelTaskCommand <>
         updateTaskTextCommand <>
         todayCommand)
    listTasksCommand :: Mod CommandFields SubCommand
    listTasksCommand =
      command "list" (info (pure ListTasks) (progDesc "List current tasks"))
    addTaskCommand :: Mod CommandFields SubCommand
    addTaskCommand = command "add" (info addOptions (progDesc "add a new task"))
    addOptions :: Parser SubCommand
    addOptions =
      AddTask <$> taskContextOption <*>
      many (textArgument (help "Text of the new task"))
    deleteTaskCommand :: Mod CommandFields SubCommand
    deleteTaskCommand =
      command "delete" (info deleteOptions (progDesc "Delete an existing task"))
    deleteOptions :: Parser SubCommand
    deleteOptions =
      DeleteTask <$> argument auto (help "ID of the task to delete")
    checkTaskCommand :: Mod CommandFields SubCommand
    checkTaskCommand =
      command "check" (info checkOptions (progDesc "Check an existing task"))
    checkOptions :: Parser SubCommand
    checkOptions = CheckTask <$> argument auto (help "ID of the task to check")
    cancelTaskCommand :: Mod CommandFields SubCommand
    cancelTaskCommand =
      command "cancel" (info cancelOptions (progDesc "Cancel an existing task"))
    cancelOptions :: Parser SubCommand
    cancelOptions =
      CancelTask <$> argument auto (help "ID of the task to cancel")
    updateTaskTextCommand :: Mod CommandFields SubCommand
    updateTaskTextCommand =
      command
        "update"
        (info updateTextOptions (progDesc "updates an existing task text"))
    updateTextOptions :: Parser SubCommand
    updateTextOptions =
      Update <$> argument auto (help "ID of the task to update") <*>
      many (textArgument (help "Text of the task"))
    todayCommand :: Mod CommandFields SubCommand
    todayCommand =
      command
        "today"
        (info
           todayOptions
           (progDesc "Generate a today summary for the given context"))
    todayOptions :: Parser SubCommand
    todayOptions =
      Today <$>
      textArgument (help "The context to generate the today message for")

update :: SubCommand -> Elapsed -> Tasks.Tasks -> Either String Tasks.Tasks
update sc currentTime tasks =
  case sc of
    AddTask taskContext textFrags ->
      Right (Tasks.addTask tasks text currentTime taskContext)
      where text = T.intercalate " " textFrags
    ListTasks -> Right tasks
    DeleteTask taskId -> Right (Tasks.removeTask tasks taskId)
    CheckTask taskId ->
      Tasks.updateTaskStatus Tasks.Done tasks taskId currentTime
    CancelTask taskId ->
      Tasks.updateTaskStatus Tasks.Cancelled tasks taskId currentTime
    Update taskId textFrags ->
      Tasks.updateTaskText text tasks taskId currentTime
      where text = T.intercalate " " textFrags
    Today context -> Right tasks

view :: SubCommand -> Elapsed -> Tasks.Tasks -> IO ()
view sc currentTime tasks =
  case sc of
    Today context -> Ui.showToday context tasks
    other -> Ui.render tasks currentTime

main :: IO ()
main = do
  (opts :: Opts) <- execParser optsParser
  currentTime <- liftIO timeCurrent
  resolvedTaskFilePath <- liftIO $ Taskfile.resolveFromEnv (taskFilePath opts)
  Taskfile.ensure resolvedTaskFilePath (Tasks.defaultTasks currentTime)
  Taskfile.loadTasks resolvedTaskFilePath >>= \case
    Left err -> Ui.displayError err
    Right tasks ->
      case update (subCommand opts) currentTime tasks of
        Right newTasks -> do
          Taskfile.create resolvedTaskFilePath newTasks
          view (subCommand opts) currentTime newTasks
        Left err -> Ui.displayError err
