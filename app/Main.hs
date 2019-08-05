{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Data.Either.Combinators (mapRight)
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
  deriving (Show)

data SubCommand
  = AddTask Text [Text]
  | ListTasks (Maybe Tasks.Context)
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
  | AddRef Refs.Service Refs.UrlTemplate
  | DeleteRef Refs.Service
  deriving (Show)

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
    reporterCommands = commandGroup "Reporters:" <> hidden <> todayCommand
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
    maybeTaskContextOption :: Parser (Maybe Text)
    maybeTaskContextOption =
      maybeTextOption
        (long "context" <> short 'c' <> value Nothing <>
         help Help.maybeContextFilter)
    taskIdArgument :: Parser Int
    taskIdArgument = argument auto (help Help.taskId)
    listTasksCommand :: Mod CommandFields SubCommand
    listTasksCommand =
      command "list" (info listTasksOptions (progDesc Help.listTasks))
    listTasksOptions :: Parser SubCommand
    listTasksOptions = ListTasks <$> maybeTaskContextOption
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
    todayOptions = Today <$> maybeTaskContextOption
    listRefsCommand :: Mod CommandFields SubCommand
    listRefsCommand =
      command "refs" (info (pure ListRefs) (progDesc Help.listRefs))
    addRefCommand :: Mod CommandFields SubCommand
    addRefCommand =
      command "set-ref" (info addRefOptions (progDesc Help.addRef))
    addRefOptions :: Parser SubCommand
    addRefOptions =
      AddRef <$> textArgument (help Help.refRepo) <*>
      textArgument (help Help.refPath)
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
        AddRef service urlTemplate ->
          mapRight
            (Taskfile.updateRefs taskfile)
            (Refs.setRef service urlTemplate currentRefs)
        DeleteRef repo -> Right (Taskfile.updateRefs taskfile newRefs)
          where newRefs = Refs.removeRef repo currentRefs

view :: SubCommand -> Elapsed -> Taskfile.Taskfile -> IO ()
view sc currentTime taskfile =
  case sc of
    Today maybeContext -> Ui.showToday maybeContext taskfile
    ListRefs -> Ui.showRefs (Taskfile.refs taskfile)
    AddRef _repo _repoPath -> Ui.showRefs (Taskfile.refs taskfile)
    DeleteRef _repo -> Ui.showRefs (Taskfile.refs taskfile)
    ListTasks maybeContext -> Ui.showTasks maybeContext taskfile currentTime
    other -> Ui.showTasks Nothing taskfile currentTime

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
