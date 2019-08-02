{-# LANGUAGE OverloadedStrings #-}

module Refs where

import Data.Aeson
import qualified Data.HashMap.Strict as Map
import Data.List as L
import Data.Text as T
import Text.Regex.PCRE

type Repo = Text

type RepoPath = Text

data Ref =
  Ref
    { repo :: Repo
    , issueNumber :: Text
    , matchedOn :: Text
    }

type RefMap = Map.HashMap Repo RepoPath

emptyRefMap :: RefMap
emptyRefMap = Map.empty

resolveRef :: Ref -> RefMap -> Text
resolveRef ref refMap =
  case Map.lookup (repo ref) refMap of
    Just repoPath -> buildRefUrl ref repoPath
    Nothing -> T.unwords ["Cannot resolve", refString]
      where refString = T.intercalate "#" [repo ref, issueNumber ref]

extractRefs :: Text -> [Ref]
extractRefs text = L.map builder matches
  where
    result :: AllTextMatches [] String
    result = unpack text =~ refMatcher
    rawMatches = getAllTextMatches result
    matches = L.map pack rawMatches
    builder match =
      let [repo, issueNo] = splitOn "#" match
       in Ref repo issueNo match

replaceRefs :: Text -> RefMap -> Text
replaceRefs text refMap = L.foldl expandRef text (extractRefs text)
  where
    expandRef t ref =
      case Map.lookup (repo ref) refMap of
        Just repoPath -> replace (matchedOn ref) (buildRefUrl ref repoPath) t
        Nothing -> t

refMatcher :: String
refMatcher = "(\\w*#\\d+)"

buildRefUrl :: Ref -> RepoPath -> Text
buildRefUrl ref repoPath =
  T.intercalate "/" ["https://github.com", repoPath, "issues", issueNumber ref]

refId :: Ref -> Text
refId ref = T.intercalate "#" [repo ref, issueNumber ref]

setRef :: Repo -> RepoPath -> RefMap -> RefMap
setRef = Map.insert

removeRef :: Repo -> RefMap -> RefMap
removeRef = Map.delete
