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
import Data.Time.Clock (UTCTime, getCurrentTime)
import Options.Applicative
import System.Directory
import qualified Tasks
import qualified Ui

data Opts =
  Opts
    { taskFilePath :: FilePath
    , subCommand :: SubCommand
    }

data SubCommand
  = AddTask [Text]
  | ListTasks
  | DeleteTask Int
  | CheckTask Int

defaultTaskFilePath :: FilePath
defaultTaskFilePath = "./tasks.json"

createTaskFile :: FilePath -> Tasks.Tasks -> IO ()
createTaskFile path tasks = I.writeFile path (encodeToLazyText tasks)

ensureTaskFile :: FilePath -> Tasks.Tasks -> IO ()
ensureTaskFile path tasks =
  ifM (doesFileExist path) (return ()) (createTaskFile path tasks)

loadTasksFromFile :: FilePath -> IO (Either String Tasks.Tasks)
loadTasksFromFile path = eitherDecode <$> B.readFile path

listTasks :: FilePath -> IO ()
listTasks taskFilePath = do
  result <- loadTasksFromFile taskFilePath
  case result of
    Left err -> Ui.displayError err
    Right tasks -> Ui.displayTasks tasks

addTask :: Text -> UTCTime -> FilePath -> IO ()
addTask text currentTime taskFilePath = do
  result <- loadTasksFromFile taskFilePath
  case result of
    Left err -> Ui.displayError err
    Right tasks -> do
      createTaskFile taskFilePath newTasks
      Ui.displayTasks newTasks
      where newTasks = Tasks.addTask tasks text currentTime

deleteTask :: Int -> FilePath -> IO ()
deleteTask taskId taskFilePath = do
  result <- loadTasksFromFile taskFilePath
  case result of
    Left err -> Ui.displayError err
    Right tasks -> do
      createTaskFile taskFilePath newTasks
      Ui.displayTasks newTasks
      where newTasks = Tasks.removeTask tasks taskId

checkTask :: Int -> UTCTime -> FilePath -> IO ()
checkTask taskId currentTime taskFilePath = do
  result <- loadTasksFromFile taskFilePath
  case result of
    Left err -> Ui.displayError err
    Right tasks ->
      case Tasks.checkTask tasks taskId currentTime of
        Left err -> Ui.displayError err
        Right newTasks -> do
          createTaskFile taskFilePath newTasks
          Ui.displayTasks newTasks

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
      hsubparser
        (listTasksCommand <> addTaskCommand <> deleteTaskCommand <>
         checkTaskCommand)
    listTasksCommand :: Mod CommandFields SubCommand
    listTasksCommand =
      command "list" (info (pure ListTasks) (progDesc "List current tasks")) <>
      command "ls" (info (pure ListTasks) (progDesc "List current tasks"))
    addTaskCommand :: Mod CommandFields SubCommand
    addTaskCommand =
      command "add" (info addOptions (progDesc "add a new task")) <>
      command "a" (info addOptions (progDesc "add a new task"))
    addOptions :: Parser SubCommand
    addOptions = AddTask <$> many (textArgument (help "Text of the new task"))
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

main :: IO ()
main = do
  (opts :: Opts) <- execParser optsParser
  currentTime <- liftIO getCurrentTime
  ensureTaskFile (taskFilePath opts) (Tasks.defaultTasks currentTime)
  case subCommand opts of
    AddTask textFrags -> addTask text currentTime (taskFilePath opts)
      where text = T.intercalate " " textFrags
    ListTasks -> listTasks (taskFilePath opts)
    DeleteTask id -> deleteTask id (taskFilePath opts)
    CheckTask id -> checkTask id currentTime (taskFilePath opts)
