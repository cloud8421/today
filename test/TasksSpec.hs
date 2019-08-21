{-# LANGUAGE OverloadedStrings #-}

module TasksSpec
  ( spec
  ) where

import Control.Monad.Reader (runReaderT)
import qualified Data.Map.Strict as Map
import Data.Text.Arbitrary ()
import qualified Tasks
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Time.Types (Elapsed(..))

currentTime :: Elapsed
currentTime = Elapsed 1566384063

instance Arbitrary Tasks.Status where
  arbitrary =
    elements [Tasks.Pending, Tasks.Progress, Tasks.Done, Tasks.Cancelled]

instance Arbitrary Tasks.Task where
  arbitrary = do
    taskStatus <- arbitrary
    taskText <- arbitrary
    Tasks.Task taskStatus taskText currentTime <$> arbitrary

spec :: Spec
spec =
  describe "Tasks" $ do
    describe "task id" $ do
      it "defaults to 1" $ Tasks.newTaskId mempty `shouldBe` 1
      it "generates a valid id" $ do
        let tasks = Tasks.add "Example" "work" mempty currentTime
        Tasks.newTaskId tasks `shouldBe` 2
    describe "add and remove tasks" $ do
      it "can add a task" $ do
        let tasks = Tasks.add "Example" "work" mempty currentTime
        Tasks.totalCount tasks `shouldBe` 1
      it "can remove a task" $ do
        let tasks = Tasks.add "Example" "work" mempty currentTime
        Tasks.totalCount (Tasks.remove tasks 1) `shouldBe` 0
    describe "update task status" $ do
      context "for an existing task" $
        it "updates the task status" $ do
          let tasks = Tasks.add "Example" "work" mempty currentTime
          Tasks.countByStatus Tasks.Pending tasks `shouldBe` 1
          let Right newTasks =
                runReaderT
                  (Tasks.updateStatus Tasks.Progress 1 tasks)
                  currentTime
          Tasks.countByStatus Tasks.Pending newTasks `shouldBe` 0
          Tasks.countByStatus Tasks.Progress newTasks `shouldBe` 1
      context "for a non existing task" $
        it "returns an error" $
        runReaderT (Tasks.updateStatus Tasks.Progress 1 mempty) currentTime `shouldBe`
        Left "Task not found"
    describe "counting tasks" $
      prop "counts by status" $
      forAll
        genTasks
        (\tasks ->
           Tasks.countByStatus Tasks.Pending tasks +
           Tasks.countByStatus Tasks.Progress tasks +
           Tasks.countByStatus Tasks.Done tasks +
           Tasks.countByStatus Tasks.Cancelled tasks ==
           Tasks.totalCount tasks)
    describe "grouping tasks" $
      prop "group by context" $
      forAll
        genTasks
        (\tasks ->
           Map.size (Map.foldl Map.union Map.empty (Tasks.groupByContext tasks)) ==
           Tasks.totalCount tasks)

genTasks :: Gen Tasks.Tasks
genTasks = arbitrary
