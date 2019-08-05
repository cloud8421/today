{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Data.Either.Combinators (mapRight)
import Data.Semigroup ((<>))
import Data.Text as T
import Options.Applicative
import qualified Refs
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
  deriving (Show)

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
  | Today (Maybe Tasks.Context)
  | ListRefs
  | AddRef Refs.Repo Refs.RepoPath
  | DeleteRef Refs.Repo
  deriving (Show)

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
      (hsubparser taskManagementCommands <|> hsubparser reporterCommands <|>
       hsubparser refManagementCommands)
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
    reporterCommands = commandGroup "Reporters:" <> todayCommand
    refManagementCommands :: Mod CommandFields SubCommand
    refManagementCommands =
      commandGroup "Refs management:" <> listRefsCommand <> addRefCommand <>
      deleteRefCommand
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
    maybeTaskContextOption :: Parser (Maybe Text)
    maybeTaskContextOption =
      option
        (eitherReader maybeContext)
        (long "context" <> short 'c' <> value Nothing <>
         help "The content for the task, e.g. work or home")
      where
        maybeContext = Right . Just . pack
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
      command "today" (info todayOptions (progDesc "Generate a today summary"))
    todayOptions :: Parser SubCommand
    todayOptions = Today <$> maybeTaskContextOption
    listRefsCommand :: Mod CommandFields SubCommand
    listRefsCommand =
      command
        "list_refs"
        (info (pure ListRefs) (progDesc "Show currently configured ref lookups"))
    addRefCommand :: Mod CommandFields SubCommand
    addRefCommand =
      command
        "add_ref"
        (info addRefOptions (progDesc "add a new ref lookup rule"))
    addRefOptions :: Parser SubCommand
    addRefOptions =
      AddRef <$> textArgument (help "The repo name to use") <*>
      textArgument (help "The github path the repo points to")
    deleteRefCommand :: Mod CommandFields SubCommand
    deleteRefCommand =
      command
        "delete_ref"
        (info deleteRefOptions (progDesc "add a new ref lookup rule"))
    deleteRefOptions :: Parser SubCommand
    deleteRefOptions =
      DeleteRef <$> textArgument (help "The repo name to delete")

update ::
     SubCommand
  -> Elapsed
  -> Taskfile.Taskfile
  -> Either String Taskfile.Taskfile
update sc currentTime taskfile =
  let currentTasks = Taskfile.tasks taskfile
      currentRefs = Taskfile.refs taskfile
   in case sc of
        AddTask taskContext textFrags ->
          Right (Taskfile.updateTasks taskfile newTasks)
          where text = T.intercalate " " textFrags
                newTasks = Tasks.add text currentTime taskContext currentTasks
        ListTasks -> Right taskfile
        DeleteTask taskId -> Right (Taskfile.updateTasks taskfile newTasks)
          where newTasks = Tasks.remove currentTasks taskId
        CheckTask taskId ->
          mapRight
            (Taskfile.updateTasks taskfile)
            (Tasks.updateStatus Tasks.Done currentTasks taskId currentTime)
        CancelTask taskId ->
          mapRight
            (Taskfile.updateTasks taskfile)
            (Tasks.updateStatus Tasks.Cancelled currentTasks taskId currentTime)
        StartTask taskId ->
          mapRight
            (Taskfile.updateTasks taskfile)
            (Tasks.updateStatus Tasks.Progress currentTasks taskId currentTime)
        PauseTask taskId ->
          mapRight
            (Taskfile.updateTasks taskfile)
            (Tasks.updateStatus Tasks.Pending currentTasks taskId currentTime)
        Update taskId textFrags ->
          mapRight
            (Taskfile.updateTasks taskfile)
            (Tasks.updateText text currentTasks taskId currentTime)
          where text = T.intercalate " " textFrags
        Move taskId context ->
          mapRight
            (Taskfile.updateTasks taskfile)
            (Tasks.updateContext context currentTasks taskId currentTime)
        Clear -> Right (Taskfile.updateTasks taskfile newTasks)
          where newTasks = Tasks.clearCompleted currentTasks
        Today _maybeContext -> Right taskfile
        ListRefs -> Right taskfile
        AddRef repo repoPath -> Right (Taskfile.updateRefs newRefs taskfile)
          where newRefs = Refs.setRef repo repoPath currentRefs
        DeleteRef repo -> Right (Taskfile.updateRefs newRefs taskfile)
          where newRefs = Refs.removeRef repo currentRefs

view :: SubCommand -> Elapsed -> Taskfile.Taskfile -> IO ()
view sc currentTime taskfile =
  case sc of
    Today maybeContext -> Ui.showToday maybeContext taskfile
    ListRefs -> Ui.showRefs (Taskfile.refs taskfile)
    AddRef _repo _repoPath -> Ui.showRefs (Taskfile.refs taskfile)
    DeleteRef _repo -> Ui.showRefs (Taskfile.refs taskfile)
    other -> Ui.showTasks taskfile currentTime

main :: IO ()
main = do
  (opts :: Opts) <- execParser optsParser
  currentTime <- liftIO timeCurrent
  resolvedTaskFilePath <- liftIO $ Taskfile.resolveFromEnv (taskFilePath opts)
  Taskfile.ensure
    resolvedTaskFilePath
    (Taskfile.new (Tasks.defaultTasks currentTime) Refs.defaultRefMap)
  Taskfile.load resolvedTaskFilePath >>= \case
    Left err -> Ui.showError err
    Right taskfile ->
      case update (subCommand opts) currentTime taskfile of
        Right newTaskfile -> do
          Taskfile.create resolvedTaskFilePath newTaskfile
          view (subCommand opts) currentTime newTaskfile
        Left err -> Ui.showError err
