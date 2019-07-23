module Lib
  ( createTaskFile
  ) where

import Data.Text

data Status
  = Pending
  | Progress
  | Done
  | Cancelled
  deriving (Read, Show, Eq)

data Task =
  Task
    { status :: Status
    , text :: Text
    }
  deriving (Read, Show, Eq)

createTaskFile :: FilePath -> IO ()
createTaskFile path = writeFile path "[]"
