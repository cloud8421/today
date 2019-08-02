{-# LANGUAGE OverloadedStrings #-}

module Refs where

import Data.Aeson
import qualified Data.HashMap.Strict as Map
import Data.Text

type Repo = Text

type RepoPath = Text

data Ref =
  Ref
    { repo :: Repo
    , issueNumber :: Text
    }

type RefMap = Map.HashMap Repo RepoPath

emptyRefMap :: RefMap
emptyRefMap = Map.empty

resolveRef :: Ref -> RefMap -> Text
resolveRef ref refMap =
  case Map.lookup (repo ref) refMap of
    Just repoPath -> buildRefUrl ref repoPath
    Nothing -> Data.Text.unwords ["Cannot resolve", refString]
      where refString = intercalate "#" [repo ref, issueNumber ref]

refMatcher :: String
refMatcher = "(\\w*#\\d+)"

buildRefUrl :: Ref -> RepoPath -> Text
buildRefUrl ref repoPath =
  intercalate "/" ["https://github.com", repoPath, "issues", issueNumber ref]

refId :: Ref -> Text
refId ref = intercalate "#" [repo ref, issueNumber ref]

setRef :: Repo -> RepoPath -> RefMap -> RefMap
setRef = Map.insert

removeRef :: Repo -> RefMap -> RefMap
removeRef = Map.delete
