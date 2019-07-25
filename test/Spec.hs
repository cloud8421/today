{-# LANGUAGE OverloadedStrings #-}

import qualified System.Hourglass as Hourglass
import qualified Tasks
import Test.Hspec

main :: IO ()
main =
  hspec $
  before Hourglass.timeCurrent $
  describe "Tasks" $
  describe "newTaskId" $ do
    it "defaults to 1" $ \_ -> Tasks.newTaskId Tasks.emptyTasks `shouldBe` 1
    it "generates a valid id" $ \currentTime -> do
      let tasks = Tasks.addTask Tasks.emptyTasks "Example" currentTime "work"
      Tasks.newTaskId tasks `shouldBe` 2
