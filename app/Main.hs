module Main where

import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy.IO as I
import Lib
import System.Console.ANSI

defaultTaskFilePath :: FilePath
defaultTaskFilePath = "./tasks.json"

createTaskFile :: FilePath -> Tasks -> IO ()
createTaskFile path tasks = I.writeFile path (encodeToLazyText tasks)

main :: IO ()
main = do
  createTaskFile defaultTaskFilePath defaultTasks
  setSGR [SetColor Foreground Vivid Red]
  print defaultTasks
  setSGR [Reset]
