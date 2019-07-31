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

create :: FilePath -> Tasks -> RefMap -> IO ()
create path tasks refMap = I.writeFile path (encodeToLazyText taskfile)
  where
    taskfile = Taskfile currentVersion tasks refMap

ensure :: FilePath -> Tasks -> RefMap -> IO ()
ensure path tasks refMap =
  unlessM (doesFileExist path) (create path tasks refMap)

load :: FilePath -> IO (Either String Taskfile)
load path = eitherDecode <$> B.readFile path

resolveFromEnv :: FilePath -> IO FilePath
resolveFromEnv fallback = fromMaybeM (return fallback) (lookupEnv "TASKFILE")
