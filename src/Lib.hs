{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Lib where

import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Data.Semigroup ((<>))
import Data.Text as T
import qualified Help
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

data SubCommand
  = AddTask Text [Text]
  | ListTasks Ui.ContextFilter
  | DeleteTask Tasks.TaskId
  | CheckTask Tasks.TaskId
  | CancelTask Tasks.TaskId
  | StartTask Tasks.TaskId
  | PauseTask Tasks.TaskId
  | Update Tasks.TaskId [Text]
  | Move Tasks.TaskId Tasks.Context
  | Clear
  | Today Ui.ContextFilter
  | OutForToday Ui.ContextFilter
  | ListRefs
  | AddRef Refs.Service Refs.UrlTemplate
  | DeleteRef Refs.Service

optsParser :: ParserInfo Opts
optsParser = info (helper <*> programOptions) description
  where
    description :: InfoMod Opts
    description = fullDesc <> progDesc Help.progDesc <> header Help.progHeader
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
    reporterCommands =
      commandGroup "Reporters:" <> hidden <> todayCommand <> outForTodayCommand
    refManagementCommands :: Mod CommandFields SubCommand
    refManagementCommands =
      commandGroup "Refs management:" <> hidden <> listRefsCommand <>
      addRefCommand <>
      deleteRefCommand
    textArgument :: Mod ArgumentFields String -> Parser Text
    textArgument = fmap pack . strArgument
    textOption :: Mod OptionFields String -> Parser Text
    textOption = fmap pack . strOption
    maybeTextOption :: Mod OptionFields (Maybe Text) -> Parser (Maybe Text)
    maybeTextOption = option maybeText
      where
        maybeText = eitherReader (Right . Just . pack)
    taskFilePathOption :: Parser FilePath
    taskFilePathOption =
      strOption
        (long "taskfile" <> short 'f' <> metavar "TASKFILE" <>
         value Taskfile.defaultPath <>
         showDefault <>
         help Help.taskfile)
    taskContextOption :: Parser Text
    taskContextOption =
      textOption
        (long "context" <> short 'c' <> value Tasks.defaultContext <>
         help Help.contextFilter)
    contextFilterOption :: Parser Ui.ContextFilter
    contextFilterOption =
      includeContextFilterOption <|> excludeContextFilterOption
      where
        includeContextFilterOption =
          option
            includeReader
            (long "include-context" <> short 'i' <> value Ui.All <>
             help Help.includeContextFilter)
        includeReader = eitherReader (Right . Ui.Include . pack)
        excludeContextFilterOption =
          option
            excludeReader
            (long "exclude-context" <> short 'e' <> value Ui.All <>
             help Help.excludeContextFilter)
        excludeReader = eitherReader (Right . Ui.Exclude . pack)
    taskIdArgument :: Parser Int
    taskIdArgument = argument auto (help Help.taskId)
    listTasksCommand :: Mod CommandFields SubCommand
    listTasksCommand =
      command "list" (info listTasksOptions (progDesc Help.listTasks))
    listTasksOptions :: Parser SubCommand
    listTasksOptions = ListTasks <$> contextFilterOption
    addTaskCommand :: Mod CommandFields SubCommand
    addTaskCommand = command "add" (info addOptions (progDesc Help.addTask))
    addOptions :: Parser SubCommand
    addOptions =
      AddTask <$> taskContextOption <*> many (textArgument (help Help.taskText))
    deleteTaskCommand :: Mod CommandFields SubCommand
    deleteTaskCommand =
      command "delete" (info deleteOptions (progDesc Help.deleteTask))
    deleteOptions :: Parser SubCommand
    deleteOptions = DeleteTask <$> taskIdArgument
    checkTaskCommand :: Mod CommandFields SubCommand
    checkTaskCommand =
      command "check" (info checkOptions (progDesc Help.checkTask))
    checkOptions :: Parser SubCommand
    checkOptions = CheckTask <$> taskIdArgument
    cancelTaskCommand :: Mod CommandFields SubCommand
    cancelTaskCommand =
      command "cancel" (info cancelOptions (progDesc Help.cancelTask))
    cancelOptions :: Parser SubCommand
    cancelOptions = CancelTask <$> taskIdArgument
    startTaskCommand :: Mod CommandFields SubCommand
    startTaskCommand =
      command "start" (info startOptions (progDesc Help.startTask))
    startOptions :: Parser SubCommand
    startOptions = StartTask <$> taskIdArgument
    pauseTaskCommand :: Mod CommandFields SubCommand
    pauseTaskCommand =
      command "pause" (info pauseOptions (progDesc Help.pauseTask))
    pauseOptions :: Parser SubCommand
    pauseOptions = PauseTask <$> taskIdArgument
    updateTaskTextCommand :: Mod CommandFields SubCommand
    updateTaskTextCommand =
      command "update" (info updateTextOptions (progDesc Help.updateTask))
    updateTextOptions :: Parser SubCommand
    updateTextOptions =
      Update <$> taskIdArgument <*> many (textArgument (help Help.taskText))
    moveTaskCommand :: Mod CommandFields SubCommand
    moveTaskCommand =
      command "move" (info moveTaskOptions (progDesc Help.moveTask))
    moveTaskOptions :: Parser SubCommand
    moveTaskOptions =
      Move <$> taskIdArgument <*> textArgument (help Help.contextMove)
    clearCommand :: Mod CommandFields SubCommand
    clearCommand =
      command "clear" (info (pure Clear) (progDesc Help.clearTasks))
    todayCommand :: Mod CommandFields SubCommand
    todayCommand = command "today" (info todayOptions (progDesc Help.today))
    todayOptions :: Parser SubCommand
    todayOptions = Today <$> contextFilterOption
    outForTodayCommand :: Mod CommandFields SubCommand
    outForTodayCommand =
      command
        "out-for-today"
        (info outForTodayOptions (progDesc Help.outForToday))
    outForTodayOptions :: Parser SubCommand
    outForTodayOptions = OutForToday <$> contextFilterOption
    listRefsCommand :: Mod CommandFields SubCommand
    listRefsCommand =
      command "refs" (info (pure ListRefs) (progDesc Help.listRefs))
    addRefCommand :: Mod CommandFields SubCommand
    addRefCommand =
      command "set-ref" (info addRefOptions (progDesc Help.addRef))
    addRefOptions :: Parser SubCommand
    addRefOptions =
      AddRef <$> textArgument (help Help.refService) <*>
      textArgument (help Help.refUrlTemplate)
    deleteRefCommand :: Mod CommandFields SubCommand
    deleteRefCommand =
      command "delete-ref" (info deleteRefOptions (progDesc Help.deleteRef))
    deleteRefOptions :: Parser SubCommand
    deleteRefOptions = DeleteRef <$> textArgument (help Help.refRepoAction)

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
        ListTasks _context -> Right taskfile
        DeleteTask taskId -> Right (Taskfile.updateTasks taskfile newTasks)
          where newTasks = Tasks.remove currentTasks taskId
        CheckTask taskId ->
            Taskfile.updateTasks taskfile <$> Tasks.updateStatus Tasks.Done  currentTime taskId currentTasks
        CancelTask taskId ->
            Taskfile.updateTasks taskfile <$> Tasks.updateStatus Tasks.Cancelled currentTime taskId currentTasks
        StartTask taskId ->
            Taskfile.updateTasks taskfile <$> Tasks.updateStatus Tasks.Progress currentTime taskId currentTasks
        PauseTask taskId ->
            Taskfile.updateTasks taskfile <$> Tasks.updateStatus Tasks.Pending currentTime taskId currentTasks
        Update taskId textFrags ->
            Taskfile.updateTasks taskfile <$> Tasks.updateText text currentTime taskId currentTasks
          where text = T.intercalate " " textFrags
        Move taskId context ->
            Taskfile.updateTasks taskfile <$> Tasks.updateContext context currentTime taskId currentTasks
        Clear -> Right (Taskfile.updateTasks taskfile newTasks)
          where newTasks = Tasks.clearCompleted currentTasks
        Today _maybeContext -> Right taskfile
        OutForToday _maybeContext -> Right taskfile
        ListRefs -> Right taskfile
        AddRef service urlTemplate ->
            Taskfile.updateRefs taskfile <$>
            Refs.setRef service urlTemplate currentRefs
        DeleteRef repo -> Right (Taskfile.updateRefs taskfile newRefs)
          where newRefs = Refs.removeRef repo currentRefs

view :: SubCommand -> Elapsed -> Taskfile.Taskfile -> IO ()
view sc currentTime taskfile =
  case sc of
    Today contextFilter -> Ui.showToday contextFilter taskfile
    OutForToday contextFilter -> Ui.showOutForToday contextFilter taskfile
    ListRefs -> Ui.showRefs (Taskfile.refs taskfile)
    AddRef _repo _repoPath -> Ui.showRefs (Taskfile.refs taskfile)
    DeleteRef _repo -> Ui.showRefs (Taskfile.refs taskfile)
    ListTasks contextFilter -> Ui.showTasks contextFilter taskfile currentTime
    other -> Ui.showTasks Ui.All taskfile currentTime

executeCommand :: IO ()
executeCommand = do
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
