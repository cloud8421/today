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
  | StartTask Tasks.TaskId
  | PauseTask Tasks.TaskId
  | Update Tasks.TaskId [Text]
  | Move Tasks.TaskId Tasks.Context
  | Clear
  | Today
  | TodayByContext Text

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
      (hsubparser taskManagementCommands <|> hsubparser reporterCommands)
    taskManagementCommands :: Mod CommandFields SubCommand
    taskManagementCommands =
      commandGroup "Task management:" <> listTasksCommand <> addTaskCommand <>
      deleteTaskCommand <>
      checkTaskCommand <>
      cancelTaskCommand <>
      startTaskCommand <>
      pauseTaskCommand <>
      updateTaskTextCommand <>
      moveTaskCommand <>
      clearCommand
    reporterCommands :: Mod CommandFields SubCommand
    reporterCommands =
      commandGroup "Reporters:" <> todayCommand <> todayByContextCommand
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
        (long "context" <> short 'c' <> value Tasks.defaultContext <>
         help "The content for the task, e.g. work or home")
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
    startTaskCommand :: Mod CommandFields SubCommand
    startTaskCommand =
      command "start" (info startOptions (progDesc "Start an existing task"))
    startOptions :: Parser SubCommand
    startOptions = StartTask <$> argument auto (help "ID of the task to start")
    pauseTaskCommand :: Mod CommandFields SubCommand
    pauseTaskCommand =
      command "pause" (info pauseOptions (progDesc "Pauses an existing task"))
    pauseOptions :: Parser SubCommand
    pauseOptions = PauseTask <$> argument auto (help "ID of the task to start")
    updateTaskTextCommand :: Mod CommandFields SubCommand
    updateTaskTextCommand =
      command
        "update"
        (info updateTextOptions (progDesc "updates an existing task text"))
    updateTextOptions :: Parser SubCommand
    updateTextOptions =
      Update <$> argument auto (help "ID of the task to update") <*>
      many (textArgument (help "Text of the task"))
    moveTaskCommand :: Mod CommandFields SubCommand
    moveTaskCommand =
      command
        "move"
        (info moveTaskOptions (progDesc "moves a task to a different context"))
    moveTaskOptions :: Parser SubCommand
    moveTaskOptions =
      Move <$> argument auto (help "ID of the task to update") <*>
      textArgument (help "Context of the task")
    clearCommand :: Mod CommandFields SubCommand
    clearCommand =
      command
        "clear"
        (info (pure Clear) (progDesc "Clears done and cancelled tasks"))
    todayCommand :: Mod CommandFields SubCommand
    todayCommand =
      command "today" (info (pure Today) (progDesc "Generate a today summary"))
    todayByContextCommand :: Mod CommandFields SubCommand
    todayByContextCommand =
      command
        "today_by_context"
        (info
           todayByContextOptions
           (progDesc "Generate a today summary for the given context"))
    todayByContextOptions :: Parser SubCommand
    todayByContextOptions =
      TodayByContext <$>
      textArgument (help "The context to generate the today message for")

update ::
     SubCommand
  -> Elapsed
  -> Taskfile.Taskfile
  -> Either String Taskfile.Taskfile
update sc currentTime taskfile =
  case sc of
    AddTask taskContext textFrags ->
      Right (Taskfile.updateTasks newTasks taskfile)
      where text = T.intercalate " " textFrags
            newTasks =
              Tasks.addTask
                text
                currentTime
                taskContext
                (Taskfile.tasks taskfile)
    ListTasks -> Right taskfile
    DeleteTask taskId -> Right (Taskfile.updateTasks newTasks taskfile)
      where newTasks = Tasks.removeTask (Taskfile.tasks taskfile) taskId
    CheckTask taskId ->
      case Tasks.updateTaskStatus
             Tasks.Done
             (Taskfile.tasks taskfile)
             taskId
             currentTime of
        Right newTasks -> Right (Taskfile.updateTasks newTasks taskfile)
        Left err -> Left err
    CancelTask taskId ->
      case Tasks.updateTaskStatus
             Tasks.Cancelled
             (Taskfile.tasks taskfile)
             taskId
             currentTime of
        Right newTasks -> Right (Taskfile.updateTasks newTasks taskfile)
        Left err -> Left err
    StartTask taskId ->
      case Tasks.updateTaskStatus
             Tasks.Progress
             (Taskfile.tasks taskfile)
             taskId
             currentTime of
        Right newTasks -> Right (Taskfile.updateTasks newTasks taskfile)
        Left err -> Left err
    PauseTask taskId ->
      case Tasks.updateTaskStatus
             Tasks.Pending
             (Taskfile.tasks taskfile)
             taskId
             currentTime of
        Right newTasks -> Right (Taskfile.updateTasks newTasks taskfile)
        Left err -> Left err
    Update taskId textFrags ->
      case Tasks.updateTaskText
             text
             (Taskfile.tasks taskfile)
             taskId
             currentTime of
        Right newTasks -> Right (Taskfile.updateTasks newTasks taskfile)
        Left err -> Left err
      where text = T.intercalate " " textFrags
    Move taskId context ->
      case Tasks.updateTaskContext
             context
             (Taskfile.tasks taskfile)
             taskId
             currentTime of
        Right newTasks -> Right (Taskfile.updateTasks newTasks taskfile)
        Left err -> Left err
    Clear -> Right (Taskfile.updateTasks newTasks taskfile)
      where newTasks = Tasks.clearCompleted (Taskfile.tasks taskfile)
    Today -> Right taskfile
    TodayByContext context -> Right taskfile

view :: SubCommand -> Elapsed -> Taskfile.Taskfile -> IO ()
view sc currentTime taskfile =
  case sc of
    Today -> Ui.showToday taskfile
    TodayByContext context -> Ui.showTodayByContext context taskfile
    other -> Ui.render taskfile currentTime

main :: IO ()
main = do
  (opts :: Opts) <- execParser optsParser
  currentTime <- liftIO timeCurrent
  resolvedTaskFilePath <- liftIO $ Taskfile.resolveFromEnv (taskFilePath opts)
  Taskfile.ensure
    resolvedTaskFilePath
    (Taskfile.new (Tasks.defaultTasks currentTime) Tasks.emptyRefMap)
  Taskfile.load resolvedTaskFilePath >>= \case
    Left err -> Ui.displayError err
    Right taskfile ->
      case update (subCommand opts) currentTime taskfile of
        Right newTaskfile -> do
          Taskfile.create resolvedTaskFilePath newTaskfile
          view (subCommand opts) currentTime newTaskfile
        Left err -> Ui.displayError err
