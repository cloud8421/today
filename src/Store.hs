{-# LANGUAGE OverloadedStrings #-}

module Store where

import Data.Time.Clock (UTCTime)
import Database.Persist
import Database.Persist.Sqlite
import Schema
import Schema.Status

seed :: UTCTime -> IO (Key Task)
seed currentTime =
  runSqlite "example.db" $ do
    runMigration migrateAll
    insert task
  where
    task =
      Task
        { taskStatus = Pending
        , taskText = "Some Text description"
        , taskContext = "work"
        , taskCreatedAt = currentTime
        }
