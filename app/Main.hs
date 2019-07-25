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
import Options.Applicative
import System.Directory
import System.Hourglass (timeCurrent)
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
  | DeleteTask Int
  | CheckTask Int
  | CancelTask Int
  | Today Text

defaultTaskFilePath :: FilePath
defaultTaskFilePath = "./tasks.json"

defaultTaskContext :: String
defaultTaskContext = "inbox"

createTaskFile :: FilePath -> Tasks.Tasks -> IO ()
createTaskFile path tasks = I.writeFile path (encodeToLazyText tasks)

ensureTaskFile :: FilePath -> Tasks.Tasks -> IO ()
ensureTaskFile path tasks =
  ifM (doesFileExist path) (return ()) (createTaskFile path tasks)

loadTasksFromFile :: FilePath -> IO (Either String Tasks.Tasks)
loadTasksFromFile path = eitherDecode <$> B.readFile path

listTasks :: Elapsed -> FilePath -> IO ()
listTasks currentTime taskFilePath = do
  result <- loadTasksFromFile taskFilePath
  case result of
    Left err -> Ui.displayError err
    Right tasks -> Ui.render tasks currentTime

addTask :: Text -> Elapsed -> FilePath -> Tasks.Context -> IO ()
addTask text currentTime taskFilePath taskContext = do
  result <- loadTasksFromFile taskFilePath
  case result of
    Left err -> Ui.displayError err
    Right tasks -> do
      createTaskFile taskFilePath newTasks
      Ui.render newTasks currentTime
      where newTasks = Tasks.addTask tasks text currentTime taskContext

deleteTask :: Int -> Elapsed -> FilePath -> IO ()
deleteTask taskId currentTime taskFilePath = do
  result <- loadTasksFromFile taskFilePath
  case result of
    Left err -> Ui.displayError err
    Right tasks -> do
      createTaskFile taskFilePath newTasks
      Ui.render newTasks currentTime
      where newTasks = Tasks.removeTask tasks taskId

updateTaskStatus :: Tasks.Status -> Int -> Elapsed -> FilePath -> IO ()
updateTaskStatus newStatus taskId currentTime taskFilePath = do
  result <- loadTasksFromFile taskFilePath
  case result of
    Left err -> Ui.displayError err
    Right tasks ->
      case Tasks.updateTaskStatus newStatus tasks taskId currentTime of
        Left err -> Ui.displayError err
        Right newTasks -> do
          createTaskFile taskFilePath newTasks
          Ui.render newTasks currentTime

showToday :: Tasks.Context -> FilePath -> IO ()
showToday context taskFilePath = do
  result <- loadTasksFromFile taskFilePath
  case result of
    Left err -> Ui.displayError err
    Right tasks -> Ui.showToday context tasks

textArgument :: Mod ArgumentFields String -> Parser Text
textArgument = fmap pack . strArgument

textOption :: Mod OptionFields String -> Parser Text
textOption = fmap pack . strOption

taskFilePathOption :: Parser FilePath
taskFilePathOption =
  strOption
    (long "taskfile" <> short 'f' <> metavar "TASKFILE" <>
     value defaultTaskFilePath <>
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
         todayCommand)
    listTasksCommand :: Mod CommandFields SubCommand
    listTasksCommand =
      command "list" (info (pure ListTasks) (progDesc "List current tasks")) <>
      command "ls" (info (pure ListTasks) (progDesc "List current tasks"))
    addTaskCommand :: Mod CommandFields SubCommand
    addTaskCommand =
      command "add" (info addOptions (progDesc "add a new task")) <>
      command "a" (info addOptions (progDesc "add a new task"))
    addOptions :: Parser SubCommand
    addOptions =
      AddTask <$> taskContextOption <*>
      many (textArgument (help "Text of the new task"))
    deleteTaskCommand :: Mod CommandFields SubCommand
    deleteTaskCommand =
      command "delete" (info deleteOptions (progDesc "Delete an existing task")) <>
      command "d" (info deleteOptions (progDesc "Delete an existing task"))
    deleteOptions :: Parser SubCommand
    deleteOptions =
      DeleteTask <$> argument auto (help "ID of the task to delete")
    checkTaskCommand :: Mod CommandFields SubCommand
    checkTaskCommand =
      command "check" (info checkOptions (progDesc "Check an existing task")) <>
      command "c" (info checkOptions (progDesc "Check an existing task"))
    checkOptions :: Parser SubCommand
    checkOptions = CheckTask <$> argument auto (help "ID of the task to check")
    cancelTaskCommand :: Mod CommandFields SubCommand
    cancelTaskCommand =
      command "cancel" (info cancelOptions (progDesc "Cancel an existing task"))
    cancelOptions :: Parser SubCommand
    cancelOptions =
      CancelTask <$> argument auto (help "ID of the task to cancel")
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

main :: IO ()
main = do
  (opts :: Opts) <- execParser optsParser
  currentTime <- liftIO timeCurrent
  ensureTaskFile (taskFilePath opts) (Tasks.defaultTasks currentTime)
  case subCommand opts of
    AddTask taskContext textFrags ->
      addTask text currentTime (taskFilePath opts) taskContext
      where text = T.intercalate " " textFrags
    ListTasks -> listTasks currentTime (taskFilePath opts)
    DeleteTask id -> deleteTask id currentTime (taskFilePath opts)
    CheckTask id ->
      updateTaskStatus Tasks.Done id currentTime (taskFilePath opts)
    CancelTask id ->
      updateTaskStatus Tasks.Cancelled id currentTime (taskFilePath opts)
    Today context -> showToday context (taskFilePath opts)
