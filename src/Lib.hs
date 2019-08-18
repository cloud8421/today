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
  Opts FilePath SubCommand

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
  | InForToday Tasks.ContextFilter
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
  commandGroup "Reporters:" <> hidden <> inForTodayCommand <> outForTodayCommand

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
listTasksCommand = command "list" (info opts (progDesc Help.listTasks))
  where
    opts :: Parser SubCommand
    opts = ListTasks <$> contextFilterOption

addTaskCommand :: Mod CommandFields SubCommand
addTaskCommand = command "add" (info opts (progDesc Help.addTask))
  where
    opts :: Parser SubCommand
    opts =
      AddTask <$> taskContextOption <*> many (strArgument (help Help.taskText))
      where
        taskContextOption :: Parser Text
        taskContextOption =
          strOption
            (long "context" <> short 'c' <> value Tasks.defaultContext <>
             help Help.contextFilter)

deleteTaskCommand :: Mod CommandFields SubCommand
deleteTaskCommand = command "delete" (info opts (progDesc Help.deleteTask))
  where
    opts :: Parser SubCommand
    opts = DeleteTask <$> taskIdArgument

checkTaskCommand :: Mod CommandFields SubCommand
checkTaskCommand = command "check" (info opts (progDesc Help.checkTask))
  where
    opts :: Parser SubCommand
    opts = CheckTask <$> taskIdArgument

cancelTaskCommand :: Mod CommandFields SubCommand
cancelTaskCommand = command "cancel" (info opts (progDesc Help.cancelTask))
  where
    opts :: Parser SubCommand
    opts = CancelTask <$> taskIdArgument

startTaskCommand :: Mod CommandFields SubCommand
startTaskCommand = command "start" (info opts (progDesc Help.startTask))
  where
    opts :: Parser SubCommand
    opts = StartTask <$> taskIdArgument

pauseTaskCommand :: Mod CommandFields SubCommand
pauseTaskCommand = command "pause" (info opts (progDesc Help.pauseTask))
  where
    opts :: Parser SubCommand
    opts = PauseTask <$> taskIdArgument

updateTaskTextCommand :: Mod CommandFields SubCommand
updateTaskTextCommand = command "update" (info opts (progDesc Help.updateTask))
  where
    opts :: Parser SubCommand
    opts = Update <$> taskIdArgument <*> many (strArgument (help Help.taskText))

moveTaskCommand :: Mod CommandFields SubCommand
moveTaskCommand = command "move" (info opts (progDesc Help.moveTask))
  where
    opts :: Parser SubCommand
    opts = Move <$> taskIdArgument <*> strArgument (help Help.contextMove)

clearCommand :: Mod CommandFields SubCommand
clearCommand = command "clear" (info (pure Clear) (progDesc Help.clearTasks))

inForTodayCommand :: Mod CommandFields SubCommand
inForTodayCommand = command "in" (info opts (progDesc Help.inForToday))
  where
    opts :: Parser SubCommand
    opts = InForToday <$> contextFilterOption

outForTodayCommand :: Mod CommandFields SubCommand
outForTodayCommand =
  command "out" (info outForTodayOptions (progDesc Help.outForToday))
  where
    outForTodayOptions :: Parser SubCommand
    outForTodayOptions = OutForToday <$> contextFilterOption

listRefsCommand :: Mod CommandFields SubCommand
listRefsCommand = command "refs" (info (pure ListRefs) (progDesc Help.listRefs))

addRefCommand :: Mod CommandFields SubCommand
addRefCommand = command "set-ref" (info opts (progDesc Help.addRef))
  where
    opts :: Parser SubCommand
    opts =
      AddRef <$> strArgument (help Help.refService) <*>
      strArgument (help Help.refUrlTemplate)

deleteRefCommand :: Mod CommandFields SubCommand
deleteRefCommand = command "delete-ref" (info opts (progDesc Help.deleteRef))
  where
    opts :: Parser SubCommand
    opts = DeleteRef <$> strArgument (help Help.refRepoAction)

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
        InForToday _maybeContext -> pure taskfile
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
  pure $
    case sc of
      InForToday contextFilter -> Ui.today contextFilter taskfile
      OutForToday contextFilter -> Ui.outForToday contextFilter taskfile
      AddRef _service _urlTemplate -> Ui.refList (Taskfile.refs taskfile)
      DeleteRef _service -> Ui.refList (Taskfile.refs taskfile)
      ListRefs -> Ui.refList (Taskfile.refs taskfile)
      ListTasks contextFilter -> Ui.taskList contextFilter taskfile currentTime
      _ -> Ui.taskList Tasks.All taskfile currentTime

executeCommand :: IO ()
executeCommand = do
  Opts taskFilePath subCommand <-
    customExecParser (prefs $ disambiguate <> showHelpOnEmpty) optsParser
  currentTime <- liftIO timeCurrent
  resolvedTaskFilePath <- liftIO $ Taskfile.resolveFromEnv taskFilePath
  Taskfile.ensure resolvedTaskFilePath (defaultTaskfile currentTime)
  render =<<
    runExceptT
      (do taskfile <- Taskfile.load resolvedTaskFilePath
          newTaskfile <- runReaderT (update subCommand taskfile) currentTime
          liftIO $ Taskfile.create resolvedTaskFilePath newTaskfile
          runReaderT (view subCommand newTaskfile) currentTime)
  where
    defaultTaskfile ct = Taskfile.new (Tasks.defaultTasks ct) Refs.defaultRefMap

render :: Either String Doc -> IO ()
render (Right doc) = putDoc doc
render (Left err) = putDoc (Ui.error err)
