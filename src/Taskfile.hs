{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Taskfile where

import Control.Monad.Extra (fromMaybeM, unlessM)
import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.IO as I
import GHC.Generics
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import Tasks (RefMap, Tasks)

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

load :: FilePath -> IO (Either String Taskfile)
load path = eitherDecode <$> B.readFile path

resolveFromEnv :: FilePath -> IO FilePath
resolveFromEnv fallback = fromMaybeM (return fallback) (lookupEnv "TASKFILE")

updateTasks :: Taskfile -> Tasks -> Taskfile
updateTasks taskfile newTasks = taskfile {tasks = newTasks}

updateRefs :: RefMap -> Taskfile -> Taskfile
updateRefs newRefMap taskfile = taskfile {refs = newRefMap}
