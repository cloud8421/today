{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Taskfile where
import Control.Monad.Except (ExceptT(ExceptT))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Extra (fromMaybeM, unlessM)
import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.IO as I
import GHC.Generics
import Refs (RefMap)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import Tasks (Tasks)

data Taskfile =
  Taskfile
    { version :: Integer
    , tasks :: Tasks
    , refs :: RefMap
    }
  deriving (Generic, Show, ToJSON, FromJSON)

currentVersion :: Integer
currentVersion = 1

defaultPath :: FilePath
defaultPath = "./tasks.json"

new :: Tasks -> RefMap -> Taskfile
new = Taskfile currentVersion

create :: FilePath -> Taskfile -> IO ()
create path taskfile = I.writeFile path (encodeToLazyText taskfile)

ensure :: FilePath -> Taskfile -> IO ()
ensure path taskfile = unlessM (doesFileExist path) (create path taskfile)

load :: FilePath -> ExceptT String IO Taskfile
load path = do
  file <- liftIO $ B.readFile path
  ExceptT $ pure $ eitherDecode file

resolveFromEnv :: FilePath -> IO FilePath
resolveFromEnv fallback = fromMaybeM (return fallback) (lookupEnv "TASKFILE")

updateTasks :: Taskfile -> Tasks -> Taskfile
updateTasks taskfile newTasks = taskfile {tasks = newTasks}

updateRefs :: Taskfile -> RefMap -> Taskfile
updateRefs taskfile newRefMap = taskfile {refs = newRefMap}
