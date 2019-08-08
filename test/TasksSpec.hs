{-# LANGUAGE OverloadedStrings #-}

module TasksSpec
  ( spec
  ) where

import Control.Monad.Reader (runReaderT)
import qualified System.Hourglass as Hourglass
import qualified Tasks
import Test.Hspec

spec :: Spec
spec =
  before Hourglass.timeCurrent $
  describe "Tasks" $ do
    describe "task id" $ do
      it "defaults to 1" $ \_ -> Tasks.newTaskId mempty `shouldBe` 1
      it "generates a valid id" $ \currentTime -> do
        let tasks = Tasks.add "Example" "work" mempty currentTime
        Tasks.newTaskId tasks `shouldBe` 2
    describe "add and remove tasks" $ do
      it "can add a task" $ \currentTime -> do
        let tasks = Tasks.add "Example" "work" mempty currentTime
        Tasks.totalCount tasks `shouldBe` 1
      it "can remove a task" $ \currentTime -> do
        let tasks = Tasks.add "Example" "work" mempty currentTime
        Tasks.totalCount (Tasks.remove tasks 1) `shouldBe` 0
    describe "update task status" $ do
      context "for an existing task" $
        it "updates the task status" $ \currentTime -> do
          let tasks = Tasks.add "Example" "work" mempty currentTime
          Tasks.countByStatus Tasks.Pending tasks `shouldBe` 1
          let Right newTasks = runReaderT (Tasks.updateStatus Tasks.Progress 1 tasks) currentTime
          Tasks.countByStatus Tasks.Pending newTasks `shouldBe` 0
          Tasks.countByStatus Tasks.Progress newTasks `shouldBe` 1
      context "for a non existing task" $
        it "returns an error" $ \currentTime ->
          runReaderT (Tasks.updateStatus Tasks.Progress 1 mempty) currentTime `shouldBe` Left "Task not found"
    describe "counting tasks" $
      it "counts by status" $ \currentTime -> do
        let tasks = Tasks.defaultTasks currentTime
        Tasks.countByStatus Tasks.Pending tasks `shouldBe` 2
        Tasks.countByStatus Tasks.Progress tasks `shouldBe` 0
        Tasks.countByStatus Tasks.Done tasks `shouldBe` 1
        Tasks.countByStatus Tasks.Cancelled tasks `shouldBe` 0
