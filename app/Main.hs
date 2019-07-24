module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.IO as I
import Lib
import System.Console.ANSI
import System.Directory

defaultTaskFilePath :: FilePath
defaultTaskFilePath = "./tasks.json"

createTaskFile :: FilePath -> Tasks -> IO ()
createTaskFile path tasks = I.writeFile path (encodeToLazyText tasks)

ensureTaskFile :: Bool -> FilePath -> Tasks -> IO ()
ensureTaskFile taskFileExists path tasks =
  if taskFileExists
    then return ()
    else createTaskFile path tasks

loadTasksFromFile :: FilePath -> IO (Either String Tasks)
loadTasksFromFile path = eitherDecode <$> B.readFile path

main :: IO ()
main = do
  taskFileExists <- liftIO $ doesFileExist defaultTaskFilePath
  ensureTaskFile taskFileExists defaultTaskFilePath defaultTasks
  loadTasks defaultTaskFilePath
  where
    loadTasks path = do
      result <- loadTasksFromFile path
      case result of
        Left err -> putStrLn err
        Right tasks -> showtasks tasks
    showtasks tasks = do
      setSGR [SetColor Foreground Vivid Red]
      print tasks
      setSGR [Reset]
