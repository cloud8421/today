{-# LANGUAGE OverloadedStrings #-}

module Refs where

import Data.Aeson
import Data.Either.Combinators (mapRight)
import qualified Data.HashMap.Strict as Map
import Data.List as L
import Data.Text as T
import qualified Data.Text.Lazy as LZ
import qualified Data.Text.Template as TPL
import Text.Printf
import Text.Regex.PCRE

type Service = Text

type Identifier = Text

type UrlTemplate = Text

data Ref =
  Ref
    { service :: Service
    , identifier :: Identifier
    , matchedOn :: Text
    }
  deriving (Show, Eq)

type RefMap = Map.HashMap Service UrlTemplate

defaultRefMap :: RefMap
defaultRefMap =
  Map.fromList [("T", "https://github.com/cloud8421/t/issues/$id")]

resolveRef :: Ref -> RefMap -> Text
resolveRef ref refMap =
  case Map.lookup (service ref) refMap of
    Just urlTemplate -> buildRefUrl ref urlTemplate
    Nothing -> T.unwords ["Cannot resolve", refId ref]

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
      case Map.lookup (service ref) refMap of
        Just urlTemplate ->
          replace (matchedOn ref) (buildRefUrl ref urlTemplate) t
        Nothing -> t

refMatcher :: String
refMatcher = "(\\w*#\\d+)"

buildRefUrl :: Ref -> UrlTemplate -> Text
buildRefUrl ref urlTemplate =
  let context _id = identifier ref
   in LZ.toStrict (TPL.substitute urlTemplate context)

refId :: Ref -> Text
refId ref = T.intercalate "#" [service ref, identifier ref]

setRef :: Service -> UrlTemplate -> RefMap -> Either String RefMap
setRef service urlTemplate refMap =
  case TPL.templateSafe urlTemplate of
    Right _template -> Right (Map.insert service urlTemplate refMap)
    Left (row, col) -> Left (printf "Template error at position %d, %d" row col)

removeRef :: Service -> RefMap -> RefMap
removeRef = Map.delete
