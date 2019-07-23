module Main where

import Lib
import System.Console.ANSI

main :: IO ()
main = do
  setSGR [SetColor Foreground Vivid Red]
  someFunc
  setSGR [Reset]
