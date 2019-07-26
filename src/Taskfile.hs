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
import Tasks (Tasks)

data Taskfile =
  Taskfile
    { version :: Integer
    , tasks :: Tasks
    }
  deriving (Generic, Show, ToJSON, FromJSON)

currentVersion :: Integer
currentVersion = 1

defaultPath :: FilePath
defaultPath = "./tasks.json"

create :: FilePath -> Tasks -> IO ()
create path tasks = I.writeFile path (encodeToLazyText taskfile)
  where
    taskfile = Taskfile currentVersion tasks

ensure :: FilePath -> Tasks -> IO ()
ensure path tasks = unlessM (doesFileExist path) (create path tasks)

load :: FilePath -> IO (Either String Taskfile)
load path = eitherDecode <$> B.readFile path

resolveFromEnv :: FilePath -> IO FilePath
resolveFromEnv fallback = fromMaybeM (return fallback) (lookupEnv "TASKFILE")
