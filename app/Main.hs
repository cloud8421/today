module Main where

import Lib
import System.Console.ANSI

defaultTaskFilePath :: FilePath
defaultTaskFilePath = "./tasks.json"

main :: IO ()
main = do
  createTaskFile defaultTaskFilePath
  setSGR [SetColor Foreground Vivid Red]
  setSGR [Reset]
