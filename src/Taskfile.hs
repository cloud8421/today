module Taskfile where

import Control.Monad.Extra (fromMaybeM, ifM)
import Data.Aeson (eitherDecode)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.IO as I
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import Tasks (Tasks)

defaultPath :: FilePath
defaultPath = "./tasks.json"

create :: FilePath -> Tasks -> IO ()
create path tasks = I.writeFile path (encodeToLazyText tasks)

ensure :: FilePath -> Tasks -> IO ()
ensure path tasks = ifM (doesFileExist path) (return ()) (create path tasks)

loadTasks :: FilePath -> IO (Either String Tasks)
loadTasks path = eitherDecode <$> B.readFile path

resolveFromEnv :: FilePath -> IO FilePath
resolveFromEnv fallback = fromMaybeM (return fallback) (lookupEnv "TASKFILE")
