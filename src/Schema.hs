{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Schema where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.TH
  ( mkDeleteCascade
  , mkMigrate
  , mkPersist
  , persistLowerCase
  , share
  , sqlSettings
  )
import GHC.Generics
import Schema.Status

type Context = Text

type Service = Text

type UrlTemplate = Text

share
  [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Task
    status Status
    text Text
    context Context
    ~createdAt UTCTime default=CURRENT_TIMESTAMP
    deriving Eq Show Generic
Ref
    Id Text sql=service
    urlTemplate UrlTemplate
|]
