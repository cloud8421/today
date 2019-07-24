module Main where

import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.IO as I
import Lib
import System.Directory
import qualified Ui

defaultTaskFilePath :: FilePath
defaultTaskFilePath = "./tasks.json"

createTaskFile :: FilePath -> Tasks -> IO ()
createTaskFile path tasks = I.writeFile path (encodeToLazyText tasks)

ensureTaskFile :: FilePath -> Tasks -> IO ()
ensureTaskFile path tasks =
  ifM (doesFileExist path) (return ()) (createTaskFile path tasks)

loadTasksFromFile :: FilePath -> IO (Either String Tasks)
loadTasksFromFile path = eitherDecode <$> B.readFile path

main :: IO ()
main = do
  ensureTaskFile defaultTaskFilePath defaultTasks
  result <- loadTasksFromFile defaultTaskFilePath
  case result of
    Left err -> Ui.displayError err
    Right tasks -> Ui.displayTasks tasks
