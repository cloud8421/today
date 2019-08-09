{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, ask, runReaderT)
import Data.Semigroup ((<>))
import Data.Text as T
import qualified Help
import Options.Applicative
import Options.Applicative.Help.Pretty (Doc, putDoc)
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
  | ListTasks Tasks.ContextFilter
  | DeleteTask Tasks.TaskId
  | CheckTask Tasks.TaskId
  | CancelTask Tasks.TaskId
  | StartTask Tasks.TaskId
  | PauseTask Tasks.TaskId
  | Update Tasks.TaskId [Text]
  | Move Tasks.TaskId Tasks.Context
  | Clear
  | Today Tasks.ContextFilter
  | OutForToday Tasks.ContextFilter
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
  commandGroup "Refs management:" <> hidden <> listRefsCommand <> addRefCommand <>
  deleteRefCommand

taskFilePathOption :: Parser FilePath
taskFilePathOption =
  strOption
    (long "taskfile" <> short 'f' <> metavar "TASKFILE" <>
     value Taskfile.defaultPath <>
     showDefault <>
     help Help.taskfile)

taskContextOption :: Parser Text
taskContextOption =
  strOption
    (long "context" <> short 'c' <> value Tasks.defaultContext <>
     help Help.contextFilter)

contextFilterOption :: Parser Tasks.ContextFilter
contextFilterOption = includeContextFilterOption <|> excludeContextFilterOption
  where
    includeContextFilterOption =
      option
        includeReader
        (long "include-context" <> short 'i' <> value Tasks.All <>
         help Help.includeContextFilter)
    includeReader = eitherReader (Right . Tasks.Include . pack)
    excludeContextFilterOption =
      option
        excludeReader
        (long "exclude-context" <> short 'e' <> value Tasks.All <>
         help Help.excludeContextFilter)
    excludeReader = eitherReader (Right . Tasks.Exclude . pack)

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
  AddTask <$> taskContextOption <*> many (strArgument (help Help.taskText))

deleteTaskCommand :: Mod CommandFields SubCommand
deleteTaskCommand =
  command "delete" (info deleteOptions (progDesc Help.deleteTask))

deleteOptions :: Parser SubCommand
deleteOptions = DeleteTask <$> taskIdArgument

checkTaskCommand :: Mod CommandFields SubCommand
checkTaskCommand = command "check" (info checkOptions (progDesc Help.checkTask))

checkOptions :: Parser SubCommand
checkOptions = CheckTask <$> taskIdArgument

cancelTaskCommand :: Mod CommandFields SubCommand
cancelTaskCommand =
  command "cancel" (info cancelOptions (progDesc Help.cancelTask))

cancelOptions :: Parser SubCommand
cancelOptions = CancelTask <$> taskIdArgument

startTaskCommand :: Mod CommandFields SubCommand
startTaskCommand = command "start" (info startOptions (progDesc Help.startTask))

startOptions :: Parser SubCommand
startOptions = StartTask <$> taskIdArgument

pauseTaskCommand :: Mod CommandFields SubCommand
pauseTaskCommand = command "pause" (info pauseOptions (progDesc Help.pauseTask))

pauseOptions :: Parser SubCommand
pauseOptions = PauseTask <$> taskIdArgument

updateTaskTextCommand :: Mod CommandFields SubCommand
updateTaskTextCommand =
  command "update" (info updateTextOptions (progDesc Help.updateTask))

updateTextOptions :: Parser SubCommand
updateTextOptions =
  Update <$> taskIdArgument <*> many (strArgument (help Help.taskText))

moveTaskCommand :: Mod CommandFields SubCommand
moveTaskCommand = command "move" (info moveTaskOptions (progDesc Help.moveTask))

moveTaskOptions :: Parser SubCommand
moveTaskOptions =
  Move <$> taskIdArgument <*> strArgument (help Help.contextMove)

clearCommand :: Mod CommandFields SubCommand
clearCommand = command "clear" (info (pure Clear) (progDesc Help.clearTasks))

todayCommand :: Mod CommandFields SubCommand
todayCommand = command "today" (info todayOptions (progDesc Help.today))

todayOptions :: Parser SubCommand
todayOptions = Today <$> contextFilterOption

outForTodayCommand :: Mod CommandFields SubCommand
outForTodayCommand =
  command "out-for-today" (info outForTodayOptions (progDesc Help.outForToday))
  where
    outForTodayOptions :: Parser SubCommand
    outForTodayOptions = OutForToday <$> contextFilterOption

listRefsCommand :: Mod CommandFields SubCommand
listRefsCommand = command "refs" (info (pure ListRefs) (progDesc Help.listRefs))

addRefCommand :: Mod CommandFields SubCommand
addRefCommand = command "set-ref" (info addRefOptions (progDesc Help.addRef))

addRefOptions :: Parser SubCommand
addRefOptions =
  AddRef <$> strArgument (help Help.refService) <*>
  strArgument (help Help.refUrlTemplate)

deleteRefCommand :: Mod CommandFields SubCommand
deleteRefCommand =
  command "delete-ref" (info deleteRefOptions (progDesc Help.deleteRef))

deleteRefOptions :: Parser SubCommand
deleteRefOptions = DeleteRef <$> strArgument (help Help.refRepoAction)

update ::
     (MonadError String m, MonadReader Elapsed m)
  => SubCommand
  -> Taskfile.Taskfile
  -> m Taskfile.Taskfile
update sc taskfile =
  let currentTasks = Taskfile.tasks taskfile
      currentRefs = Taskfile.refs taskfile
   in case sc of
        AddTask taskContext textFrags -> do
          let text = T.intercalate " " textFrags
          newTasks <- Tasks.add text taskContext currentTasks
          pure (Taskfile.updateTasks taskfile newTasks)
        ListTasks _context -> pure taskfile
        DeleteTask taskId -> do
          let newTasks = Tasks.remove currentTasks taskId
          pure (Taskfile.updateTasks taskfile newTasks)
        CheckTask taskId ->
          Taskfile.updateTasks taskfile <$>
          Tasks.updateStatus Tasks.Done taskId currentTasks
        CancelTask taskId ->
          Taskfile.updateTasks taskfile <$>
          Tasks.updateStatus Tasks.Cancelled taskId currentTasks
        StartTask taskId ->
          Taskfile.updateTasks taskfile <$>
          Tasks.updateStatus Tasks.Progress taskId currentTasks
        PauseTask taskId ->
          Taskfile.updateTasks taskfile <$>
          Tasks.updateStatus Tasks.Pending taskId currentTasks
        Update taskId textFrags ->
          Taskfile.updateTasks taskfile <$>
          Tasks.updateText text taskId currentTasks
          where text = T.intercalate " " textFrags
        Move taskId context ->
          Taskfile.updateTasks taskfile <$>
          Tasks.updateContext context taskId currentTasks
        Clear -> pure (Taskfile.updateTasks taskfile newTasks)
          where newTasks = Tasks.clearCompleted currentTasks
        Today _maybeContext -> pure taskfile
        OutForToday _maybeContext -> pure taskfile
        ListRefs -> pure taskfile
        AddRef service urlTemplate ->
          Taskfile.updateRefs taskfile <$>
          Refs.setRef service urlTemplate currentRefs
        DeleteRef repo -> pure (Taskfile.updateRefs taskfile newRefs)
          where newRefs = Refs.removeRef repo currentRefs

view :: MonadReader Elapsed m => SubCommand -> Taskfile.Taskfile -> m Doc
view sc taskfile = do
  currentTime <- ask
  case sc of
    Today contextFilter -> pure $ Ui.today contextFilter taskfile
    OutForToday contextFilter -> pure $ Ui.outForToday contextFilter taskfile
    AddRef _service _urlTemplate -> pure $ Ui.refList (Taskfile.refs taskfile)
    DeleteRef _service -> pure $ Ui.refList (Taskfile.refs taskfile)
    ListRefs -> pure $ Ui.refList (Taskfile.refs taskfile)
    ListTasks contextFilter ->
      pure $ Ui.taskList contextFilter taskfile currentTime
    _ -> pure $ Ui.taskList Tasks.All taskfile currentTime

executeCommand :: IO ()
executeCommand = do
  (opts :: Opts) <-
    customExecParser (prefs $ disambiguate <> showHelpOnEmpty) optsParser
  currentTime <- liftIO timeCurrent
  resolvedTaskFilePath <- liftIO $ Taskfile.resolveFromEnv (taskFilePath opts)
  Taskfile.ensure
    resolvedTaskFilePath
    (Taskfile.new (Tasks.defaultTasks currentTime) Refs.defaultRefMap)
  render =<<
    runExceptT
      (do taskfile <- Taskfile.load resolvedTaskFilePath
          runReaderT
            (update (subCommand opts) taskfile >>= view (subCommand opts))
            currentTime)

render :: Either String Doc -> IO ()
render (Right doc) = putDoc doc
render (Left err) = putDoc (Ui.error err)
