{-# LANGUAGE OverloadedStrings #-}

module Format where

import Formatting

minute :: Int -> String
minute = formatToString (int % "m")

hour :: Int -> String
hour = formatToString (int % "h")

day :: Int -> String
day = formatToString (int % "d")

templateError :: Int -> Int -> String
templateError =
  formatToString ("Template error at position " % int % ", " % int)

taskPercentDone :: Float -> String
taskPercentDone = formatToString (fixed 0 % "% of all tasks complete.")

groupCount :: Int -> String
groupCount = formatToString int

groupCountOverTotal :: Int -> Int -> String
groupCountOverTotal = formatToString ("[" % int % "/" % int % "]")

paddedTaskId :: Int -> String
paddedTaskId = formatToString (left 3 ' ' % ".")
